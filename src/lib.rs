use std::sync::OnceLock;

use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing, Router,
};
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use yrs::{updates::decoder::Decode, Array, ReadTxn, Text, Transact, WriteTxn};

mod hobo;

emacs::plugin_is_GPL_compatible!();

emacs::use_symbols!(hobo_public_path
                    hobo_server_bind_address
                    hobo_display_error
                    symbol_name
                    symbol_value
                    object_intervals
                    eq);

struct HoboState {
    runtime: tokio::runtime::Runtime,
    server_task: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    doc: yrs::Doc,
}

static STATE: OnceLock<HoboState> = OnceLock::new();

static LOGGER: OnceLock<hobo::logger::TcpLoggerServer> = OnceLock::new();

// TODO Initialize in the start function
#[emacs::module(name = "hobors", separator = "--")]
fn init(_env: &emacs::Env) -> emacs::Result<()> {
    let runtime = tokio::runtime::Runtime::new()?;

    let doc = yrs::Doc::with_options(yrs::Options {
        offset_kind: yrs::OffsetKind::Utf16,
        ..yrs::Options::default()
    });

    {
        let mut txn = doc.transact_mut();
        txn.get_or_insert_array("buffers");
        txn.commit();
    }

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
    let logger = hobo::logger::TcpLoggerServer::new()?;
    let addr = logger.server_addr();

    logger.init(log::LevelFilter::Info)?;

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
    let router = router(&public_path);

    runtime.block_on(async {
        let mut server_task = server_task.lock().await;

        if server_task.is_some() {
            return Err(anyhow::anyhow!("Hobo already running"));
        }

        let listener = TcpListener::bind(&bind_address).await?;

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

    log::info!(public_path, bind_address; "Hobo started");

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
    begin: u32,
    removed_length: u32,
    added_text: String,
) -> emacs::Result<()> {
    let HoboState { doc, .. } = get_state()?;

    log::debug!(buffer_name, begin, removed_length, added_text_chars = added_text.chars().count(); "Updating buffer");

    let mut txn = doc.transact_mut();

    let buffer = txn
        .get_text(buffer_name.clone())
        .ok_or_else(|| anyhow::anyhow!("Buffer {} not initialized before update", buffer_name))?;

    if removed_length > 0 {
        buffer.remove_range(&mut txn, begin - 1, removed_length);
    }

    if !added_text.is_empty() {
        buffer.insert(&mut txn, begin - 1, &added_text);
    }

    txn.commit();

    log::debug!(buffer_name; "Buffer updated");

    Ok(())
}

#[emacs::defun]
fn reset_buffer(
    _env: &emacs::Env,
    buffer_name: String,
    content: hobo::emacs::StringWithProperties,
) -> emacs::Result<()> {
    let buffer_string = content.string();

    log::debug!(buffer_name, content_chars = buffer_string.chars().count(); "Resetting buffer");

    let HoboState { doc, .. } = get_state()?;
    let mut txn = doc.transact_mut();

    let buffers = txn
        .get_array("buffers")
        .ok_or_else(|| anyhow::anyhow!("Doc missing buffers array"))?;

    let buffer = txn.get_or_insert_text(buffer_name.clone());
    let len = buffer.len(&mut txn);

    if len > 0 {
        buffer.remove_range(&mut txn, 0, len);
    }

    if !buffer_string.is_empty() {
        buffer.insert(&mut txn, 0, buffer_string);
    }

    if !buffers
        .iter(&mut txn)
        .any(|b| b.cast::<String>().is_ok_and(|b| b == buffer_name))
    {
        buffers.push_front(&mut txn, yrs::In::from(buffer_name.clone()));
    }

    txn.commit();

    log::debug!(buffer_name; "Buffer reset complete");

    Ok(())
}

#[emacs::defun]
fn kill_buffer(_env: &emacs::Env, buffer_name: String) -> emacs::Result<()> {
    log::info!(buffer_name; "Killing buffer");

    let HoboState { doc, .. } = get_state()?;
    let mut txn = doc.transact_mut();

    let buffers = txn
        .get_array("buffers")
        .ok_or_else(|| anyhow::anyhow!("Doc missing buffers array"))?;

    let index = buffers
        .iter(&mut txn)
        .enumerate()
        .find(|(_, b)| b.clone().cast::<String>().is_ok_and(|b| b == buffer_name))
        .map(|(index, _)| index);

    if let Some(index) = index {
        buffers.remove(&mut txn, index as u32);
    }

    txn.commit();

    Ok(())
}

fn router(public_path: &str) -> Router {
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

            log::trace!(message:?; "Sent message");
        }
    });

    let client_messages_task = tokio::spawn(async move {
        while let Some(Ok(socket_message)) = socket_receiver.next().await {
            match socket_message {
                axum::extract::ws::Message::Text(text) => {
                    match serde_json::from_str::<ClientMessage>(&text) {
                        Ok(message) => {
                            log::trace!(message:?; "Received message");

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

                    // TODO Need to send deltas to Emacs
                    // Start a TCP server to send them to and use make-network-process to handle modifying buffers
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
