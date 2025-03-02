use std::{
    sync::OnceLock,
    time::{Duration, Instant},
};

use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing, Router,
};
use emacs::IntoLisp;
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use yrs::{updates::decoder::Decode, ReadTxn, Text, Transact};

emacs::plugin_is_GPL_compatible!();

emacs::use_symbols!(hobo_public_path
                    hobo_server_bind_address
                    hobo_display_error
                    symbol_value);

struct HoboState {
    runtime: tokio::runtime::Runtime,
    logger: HoboLogger,
    server_task: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    doc: yrs::Doc,
}

static STATE: OnceLock<HoboState> = OnceLock::new();

// TODO Initialize in the start function
#[emacs::module(name = "hobors", separator = "--")]
fn init(_env: &emacs::Env) -> emacs::Result<()> {
    let runtime = tokio::runtime::Runtime::new()?;
    let logger = HoboLogger::new();
    let doc = yrs::Doc::new();

    STATE
        .set(HoboState {
            runtime,
            logger,
            server_task: tokio::sync::Mutex::new(None),
            doc,
        })
        .map_err(|_| anyhow::anyhow!("Failed to initialize global state: already initialized"))?;

    log::set_logger(&STATE.get().unwrap().logger)
        .map(|()| log::set_max_level(log::LevelFilter::Debug))
        .map_err(|err| anyhow::anyhow!(err))?;

    Ok(())
}

#[emacs::defun]
fn start(env: &emacs::Env) -> emacs::Result<()> {
    let HoboState {
        runtime,
        server_task,
        ..
    } = get_state()?;

    let public_path = get_symbol_value::<String>(env, hobo_public_path)?;
    let bind_address = get_symbol_value::<String>(env, hobo_server_bind_address)?;
    let router = router(public_path);

    runtime.block_on(async {
        let mut server_task = server_task.lock().await;

        if server_task.is_some() {
            return Err(anyhow::anyhow!("Hobo already running"));
        }

        let listener = TcpListener::bind(bind_address).await?;

        server_task.replace(runtime.spawn(async move {
            if let Err(err) = axum::serve(listener, router).await {
                log::error!("Server quit with error: {}", err);
            }
        }));

        Ok(())
    })?;

    env.message(format!(
        "Hobo started on {}",
        get_symbol_value::<String>(env, hobo_server_bind_address)?
    ))?;

    Ok(())
}

#[emacs::defun]
fn stop(env: &emacs::Env) -> emacs::Result<()> {
    let HoboState {
        runtime,
        server_task,
        ..
    } = get_state()?;

    // TODO Shutdown runtime instead
    runtime.block_on(async {
        if let Some(server_task) = server_task.lock().await.take() {
            server_task.abort();
        }
    });

    env.message("Hobo stopped")?;

    Ok(())
}

#[emacs::defun]
fn consume_logs(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let HoboState { logger, .. } = get_state()?;

    env.list(
        &logger
            .consume_logs()
            .into_iter()
            .map(|log| log.into_lisp(env))
            .collect::<emacs::Result<Vec<emacs::Value<'_>>>>()?,
    )
}

#[emacs::defun]
fn update_buffer(
    _env: &emacs::Env,
    buffer_name: String,
    start: u32,
    length: u32,
    text: String,
) -> emacs::Result<()> {
    let HoboState { doc, .. } = get_state()?;

    let buffer = doc.get_or_insert_text(buffer_name.clone());

    let mut txn = doc.transact_mut();

    if length > 0 {
        buffer.remove_range(&mut txn, start, length);
    }

    if text.len() > 0 {
        buffer.insert(&mut txn, start, &text);
    }

    txn.commit();

    log::debug!(
        "Updated buffer {} - start: {}, length: {}, text: {}",
        buffer_name,
        start,
        length,
        text
    );

    Ok(())
}

fn router(public_path: String) -> Router {
    Router::new()
        .route(
            "/socket",
            routing::get(|upgrade: WebSocketUpgrade| async {
                upgrade.on_upgrade(|socket| handle_websocket(socket))
            }),
        )
        .fallback_service(ServeDir::new(public_path))
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type")]
enum ServerMessage {
    Diff { diff: Vec<u8> },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type")]
enum ClientMessage {
    Diff { diff: Vec<u8> },
}

async fn handle_websocket(socket: WebSocket) {
    log::info!("Client connected");

    let HoboState { doc, .. } = match get_state() {
        Ok(state) => state,
        Err(err) => {
            log::error!("Error getting Hobo state: {}", err);
            return;
        }
    };

    let (mut socket_sender, mut socket_receiver) = socket.split();

    let (server_messages_tx, mut server_messages_rx) =
        tokio::sync::mpsc::unbounded_channel::<ServerMessage>();

    let (client_messages_tx, mut client_messages_rx) =
        tokio::sync::mpsc::unbounded_channel::<ClientMessage>();

    let server_messages_task = tokio::spawn(async move {
        while let Some(message) = server_messages_rx.recv().await {
            match serde_json::to_string(&message) {
                Ok(text) => {
                    if let Err(err) = socket_sender
                        .send(axum::extract::ws::Message::text(text))
                        .await
                    {
                        log::error!("Error sending server message: {}", err);
                    }
                }
                Err(err) => {
                    log::error!("Error serializing server message: {}", err);
                    continue;
                }
            };

            log::debug!("Sent message: {:?}", message);
        }
    });

    let client_messages_tx1 = client_messages_tx.clone();

    let client_messages_task = tokio::spawn(async move {
        while let Some(Ok(socket_message)) = socket_receiver.next().await {
            match socket_message {
                axum::extract::ws::Message::Text(text) => {
                    match serde_json::from_str::<ClientMessage>(&text) {
                        Ok(message) => {
                            if let Err(err) = client_messages_tx1.send(message) {
                                log::error!("Error sending client message: {}", err);
                            }
                        }
                        Err(err) => {
                            log::error!("Error parsing client message: {}", err);
                        }
                    };
                }

                axum::extract::ws::Message::Close(_) => break,

                _ => {}
            }
        }
    });

    let client_messages_tx2 = client_messages_tx.clone();

    let sync_task: tokio::task::JoinHandle<emacs::Result<()>> = tokio::spawn(async move {
        let initial_diff = doc.transact().encode_diff_v1(&yrs::StateVector::default());

        server_messages_tx.send(ServerMessage::Diff { diff: initial_diff })?;

        doc.observe_update_v1(move |_txn, event| {
            let diff = event.update.clone();

            if let Err(err) = client_messages_tx2.send(ClientMessage::Diff { diff }) {
                log::error!("Error handling document update: {}", err);
            }
        })?;

        while let Some(message) = client_messages_rx.recv().await {
            match message {
                ClientMessage::Diff { diff } => {
                    let mut txn = doc.transact_mut();
                    txn.apply_update(yrs::Update::decode_v1(&diff)?)?;
                    txn.commit();
                }
            }
        }

        Ok(())
    });

    tokio::select! {
        _ = server_messages_task => {}
        _ = client_messages_task => {}
        Err(err) = sync_task => {
            log::error!("Error synchronizing with client: {}", err);
        }
    }

    log::info!("Closing WebSocket");
}

fn get_state<'a>() -> emacs::Result<&'a HoboState> {
    STATE
        .get()
        .ok_or_else(|| anyhow::anyhow!("State not initialized"))
}

fn get_symbol_value<'a, T: emacs::FromLisp<'a>>(
    env: &'a emacs::Env,
    symbol: impl emacs::IntoLisp<'a>,
) -> emacs::Result<T> {
    symbol_value.call(env, (symbol,))?.into_rust()
}

// TODO Log via a TCP server and use make-network-process to filter logs asynchronously
struct HoboLogger {
    tx: tokio::sync::mpsc::UnboundedSender<String>,
    rx: tokio::sync::RwLock<tokio::sync::mpsc::UnboundedReceiver<String>>,
}

impl log::Log for HoboLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            self.tx.send(self.format_record(record)).unwrap();
        }
    }

    fn flush(&self) {}
}

impl HoboLogger {
    fn new() -> Self {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<String>();

        Self {
            tx,
            rx: tokio::sync::RwLock::new(rx),
        }
    }

    fn format_record(&self, record: &log::Record) -> String {
        format!(
            "[{} {} {}] {}",
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S%.3f"),
            record.level(),
            record.module_path().unwrap_or("<unknown>"),
            record.args()
        )
    }

    fn consume_logs(&self) -> Vec<String> {
        let mut rx = self.rx.blocking_write();
        let mut logs = Vec::<String>::new();
        let deadline = Instant::now() + Duration::from_millis(100);

        while Instant::now() < deadline {
            match rx.try_recv() {
                Ok(log) => {
                    logs.push(log);
                }

                Err(tokio::sync::mpsc::error::TryRecvError::Disconnected) => {
                    logs.push(
                        self.format_record(
                            &log::Record::builder()
                                .level(log::Level::Error)
                                .args(format_args!("Logger channel closed unexpectedly"))
                                .build(),
                        ),
                    );
                }

                Err(tokio::sync::mpsc::error::TryRecvError::Empty) => break,
            }
        }

        logs
    }
}
