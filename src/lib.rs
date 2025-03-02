use std::sync::OnceLock;

use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing, Router,
};
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use yrs::{updates::decoder::Decode, ReadTxn, Text, Transact};

mod logger;

emacs::plugin_is_GPL_compatible!();

emacs::use_symbols!(hobo_public_path
                    hobo_server_bind_address
                    hobo_display_error
                    symbol_value);

struct HoboState {
    runtime: tokio::runtime::Runtime,
    server_task: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    doc: yrs::Doc,
}

static STATE: OnceLock<HoboState> = OnceLock::new();

static LOGGER: OnceLock<logger::TcpLoggerServer> = OnceLock::new();

// TODO Initialize in the start function
#[emacs::module(name = "hobors", separator = "--")]
fn init(_env: &emacs::Env) -> emacs::Result<()> {
    let runtime = tokio::runtime::Runtime::new()?;
    let doc = yrs::Doc::new();

    STATE
        .set(HoboState {
            runtime,
            server_task: tokio::sync::Mutex::new(None),
            doc,
        })
        .map_err(|_| anyhow::anyhow!("Failed to initialize global state: already initialized"))?;

    Ok(())
}

#[emacs::defun]
fn init_logger(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let logger = logger::TcpLoggerServer::new()?;
    let addr = logger.server_addr();

    logger.init(log::LevelFilter::Debug)?;

    LOGGER
        .set(logger)
        .map_err(|_| anyhow::anyhow!("Logger already initialized"))?;

    env.list((addr.ip().to_string(), addr.port()))
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

            log::debug!(message:?; "Sent message");
        }
    });

    let client_messages_task = tokio::spawn(async move {
        while let Some(Ok(socket_message)) = socket_receiver.next().await {
            match socket_message {
                axum::extract::ws::Message::Text(text) => {
                    match serde_json::from_str::<ClientMessage>(&text) {
                        Ok(message) => {
                            log::debug!(message:?; "Received message");

                            if let Err(err) = client_messages_tx.send(message) {
                                log::error!(err:err; "Error sending client message");
                            }
                        }

                        Err(err) => {
                            log::error!(err:err; "Error parsing client message");
                        }
                    };
                }

                axum::extract::ws::Message::Close(_) => break,

                _ => {}
            }
        }
    });

    let sync_task: tokio::task::JoinHandle<emacs::Result<()>> = tokio::spawn(async move {
        let initial_diff = doc.transact().encode_diff_v1(&yrs::StateVector::default());

        server_messages_tx.send(ServerMessage::Diff { diff: initial_diff })?;

        doc.observe_update_v1_with("foo", move |txn, event| {
            if let Some(origin) = txn.origin() {
                if *origin == yrs::Origin::from("client") {
                    return;
                }
            }

            let diff = event.update.clone();

            log::debug!("Document updated");

            if let Err(err) = server_messages_tx.send(ServerMessage::Diff { diff }) {
                log::error!(err:err; "Error handling document update");
                return;
            }
        })?;

        while let Some(message) = client_messages_rx.recv().await {
            match message {
                ClientMessage::Diff { diff } => {
                    log::debug!("Applying diff");
                    let mut txn = doc.transact_mut_with("client");
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
            log::error!(err:err; "Error synchronizing with client");
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
