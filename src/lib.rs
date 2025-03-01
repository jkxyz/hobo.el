use std::{
    sync::OnceLock,
    time::{Duration, Instant},
};

use axum::{
    extract::{
        ws::{Message, WebSocket},
        WebSocketUpgrade,
    },
    routing, Router,
};
use emacs::IntoLisp;
use futures_util::{SinkExt, StreamExt};
use serde_json::json;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use yrs::{ReadTxn, Text, Transact};

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
        .map(|()| log::set_max_level(log::LevelFilter::Info))
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

async fn handle_websocket(socket: WebSocket) {
    let HoboState { doc, .. } = get_state().unwrap();

    let (mut sender, mut receiver) = socket.split();

    let diff = doc.transact().encode_diff_v1(&yrs::StateVector::default());

    sender
        .send(Message::text(
            json!({"type": "DIFF", "diff": diff}).to_string(),
        ))
        .await
        .unwrap();

    let (diffs_tx, mut diffs_rx) = tokio::sync::mpsc::unbounded_channel::<Vec<u8>>();

    doc.observe_update_v1_with("foo", move |_txn, event| {
        log::info!("Doc updated");
        diffs_tx.send(event.update.to_owned()).unwrap();
    })
    .unwrap();

    let sender_task = tokio::spawn(async move {
        while let Some(diff) = diffs_rx.recv().await {
            sender
                .send(Message::text(
                    json!({"type": "DIFF", "diff": diff}).to_string(),
                ))
                .await
                .unwrap();
        }
    });

    while let Some(Ok(msg)) = receiver.next().await {
        match msg {
            Message::Close(_) => break,
            Message::Text(text) => {
                log::info!("Message received: {}", text);
            }
            _ => continue,
        }
    }
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
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Info
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
