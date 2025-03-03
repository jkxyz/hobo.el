use std::sync::OnceLock;

use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing, Router,
};
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
use yrs::{updates::decoder::Decode, GetString, ReadTxn, Text, Transact};

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
    begin: u32,
    removed_length: u32,
    added_text: String,
) -> emacs::Result<()> {
    log::info!(buffer_name, begin, removed_length, added_text_len = added_text.len(); "Updating buffer");
    
    let HoboState { doc, .. } = get_state()?;
    let buffer = doc.get_or_insert_text(buffer_name.clone());
    
    // Get current content
    let txn = doc.transact();
    let current_content = buffer.get_string(&txn);
    let char_count = current_content.chars().count();
    drop(txn);
    
    // Log buffer size metrics at info level, content at debug level
    log::info!(bytes = current_content.len(), chars = char_count; "Buffer size");
    log::debug!(content = current_content.as_str(); "Buffer content");
    
    // Handle completely empty buffer case
    if char_count == 0 && begin == 1 && removed_length == 0 {
        log::debug!("Inserting into empty buffer");
        let mut txn = doc.transact_mut();
        buffer.insert(&mut txn, 0, &added_text);
        txn.commit();
        log::info!(buffer_name; "Empty buffer updated");
        return Ok(());
    }
    
    // Bounds checking
    if begin < 1 {
        return Err(anyhow::anyhow!("begin must be at least 1"));
    }
    
    let start_char_index = begin - 1;
    
    if start_char_index as usize > char_count {
        return Err(anyhow::anyhow!("begin out of bounds: {} > {}", begin, char_count + 1));
    }
    
    if (start_char_index + removed_length) as usize > char_count {
        return Err(anyhow::anyhow!("removed_length out of bounds: {} + {} > {}", 
                                  start_char_index, removed_length, char_count));
    }
    
    // Calculate character offsets
    let mut chars = current_content.chars().collect::<Vec<_>>();
    
    // Perform the operation on our local representation
    if removed_length > 0 {
        // Remove characters
        chars.splice(
            start_char_index as usize..(start_char_index + removed_length) as usize, 
            std::iter::empty()
        );
    }
    
    // Insert the new text
    if !added_text.is_empty() {
        // Convert start_char_index to correct position in possibly modified chars
        let insert_pos = start_char_index as usize;
        if insert_pos <= chars.len() {
            // Insert at specific position
            let added_chars = added_text.chars();
            chars.splice(insert_pos..insert_pos, added_chars);
        } else {
            // Append at the end
            chars.extend(added_text.chars());
        }
    }
    
    // Convert back to string
    let new_content: String = chars.into_iter().collect();
    
    // Determine if we should use full buffer replacement or try to do partial updates
    let use_full_replacement = 
        // 1. If we're appending at the end (known problematic case)
        (start_char_index as usize == char_count && removed_length == 0) ||
        // 2. If the buffer content has character encoding issues (international chars)
        current_content.chars().any(|c| c.len_utf8() > 1 || c.len_utf16() > 1) ||
        // 3. If we're doing a replacement that spans a significant portion
        (removed_length > 0 && added_text.len() > 0);
    
    if use_full_replacement {
        log::info!("Using full buffer replacement");
        log::debug!("Reason: complex edits or special characters");
        
        // Replace the entire buffer content
        let mut txn = doc.transact_mut();
        let len = buffer.len(&mut txn);
        
        if len > 0 {
            buffer.remove_range(&mut txn, 0, len);
        }
        
        buffer.insert(&mut txn, 0, &new_content);
        txn.commit();
        
        log::info!(buffer_name; "Buffer replaced");
        Ok(())
    } else {
        // Try to do the operation directly using Yrs operations
        // This is the optimistic path for simple cases
        log::debug!("Attempting normal buffer operations");
        
        let mut txn = doc.transact_mut();
        
        // First, try removing content if needed
        if removed_length > 0 {
            log::debug!("Removing range: start={}, length={}", start_char_index, removed_length);
            
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                buffer.remove_range(&mut txn, start_char_index, removed_length);
            })) {
                Ok(_) => log::debug!("Removal successful"),
                Err(_) => {
                    // Fall back to full replacement on error
                    drop(txn);
                    log::info!("Removal failed, falling back to full replacement");
                    return reset_buffer(_env, buffer_name, new_content);
                }
            }
        }
        
        // Then try inserting new content if needed
        if !added_text.is_empty() {
            log::debug!("Inserting text at position: {}", start_char_index);
            
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                buffer.insert(&mut txn, start_char_index, &added_text);
            })) {
                Ok(_) => log::debug!("Insertion successful"),
                Err(_) => {
                    // Fall back to full replacement on error
                    drop(txn);
                    log::info!("Insertion failed, falling back to full replacement");
                    return reset_buffer(_env, buffer_name, new_content);
                }
            }
        }
        
        txn.commit();
        log::info!(buffer_name; "Buffer updated with partial edits");
        Ok(())
    }
}

// Helper function for resetting buffer content
#[emacs::defun]
fn reset_buffer(
    _env: &emacs::Env,
    buffer_name: String,
    content: String,
) -> emacs::Result<()> {
    log::info!(buffer_name, content_len = content.len(); "Resetting buffer");
    
    let HoboState { doc, .. } = get_state()?;
    let buffer = doc.get_or_insert_text(buffer_name.clone());
    let mut txn = doc.transact_mut();
    let len = buffer.len(&mut txn);
    
    if len > 0 {
        buffer.remove_range(&mut txn, 0, len);
    }
    
    if !content.is_empty() {
        buffer.insert(&mut txn, 0, &content);
    }
    
    txn.commit();
    log::info!(buffer_name; "Buffer reset complete");
    Ok(())
}

#[emacs::defun]
fn kill_buffer(_env: &emacs::Env, buffer_name: String) -> emacs::Result<()> {
    log::info!(buffer_name; "Killing buffer");

    let HoboState { doc, .. } = get_state()?;

    let buffer = doc.get_or_insert_text(buffer_name.clone());

    let mut txn = doc.transact_mut();
    let len = buffer.len(&mut txn);

    buffer.remove_range(&mut txn, 0, len);

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
