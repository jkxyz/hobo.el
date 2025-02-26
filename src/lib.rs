use axum::{
    extract::ws::{Message, WebSocket, WebSocketUpgrade},
    Router,
};
use emacs::{defun, Env, Result, Value};
use futures_util::{sink::SinkExt, stream::StreamExt};
use std::{sync::Mutex, time::Duration};
use tokio::sync::{broadcast, mpsc};
use tower_http::services::ServeDir;

emacs::use_symbols!(hobo_public_path
                    hobo_server_bind_address
                    symbol_value
                    get_buffer_create
                    switch_to_buffer
                    make_local_variable
                    add_hook
                    after_change_functions
                    hobors_handle_buffer_change
                    buffer_string);

emacs::plugin_is_GPL_compatible!();

struct State {
    runtime: tokio::runtime::Runtime,
    tx: broadcast::Sender<String>,
}

static STATE: Mutex<Option<State>> = Mutex::new(None);

#[emacs::module(name = "hobors")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

fn get_symbol_value<'a, T: emacs::FromLisp<'a>>(
    env: &'a Env,
    symbol: impl emacs::IntoLisp<'a>,
) -> Result<T> {
    symbol_value.call(env, (symbol,))?.into_rust()
}

#[defun]
fn start(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();

    if state.is_some() {
        return env.message("HOBO already started");
    }

    let runtime = tokio::runtime::Runtime::new().unwrap();

    let public_path: String = get_symbol_value(env, hobo_public_path)?;
    let bind_address: String = get_symbol_value(env, hobo_server_bind_address)?;
    let bind_address_clone = bind_address.clone();

    let (tx, _) = broadcast::channel::<String>(100);

    {
        let tx = tx.clone();

        runtime.spawn(async move {
            let app = Router::new()
                .route(
                    "/ws",
                    axum::routing::get(move |ws: WebSocketUpgrade| {
                        let tx = tx.clone();
                        async move { ws.on_upgrade(move |socket| handle_websocket(socket, tx)) }
                    }),
                )
                .fallback_service(ServeDir::new(public_path));

            let listener = tokio::net::TcpListener::bind(&bind_address).await.unwrap();
            axum::serve(listener, app).await.unwrap();
        });
    }

    *state = Some(State { runtime, tx });

    let buffer = get_buffer_create.call(env, ("*hobo*",))?;
    switch_to_buffer.call(env, (buffer,))?;
    make_local_variable.call(env, (after_change_functions,))?;
    add_hook.call(
        env,
        (
            after_change_functions,
            hobors_handle_buffer_change,
            false,
            true,
        ),
    )?;

    env.message(format!("HOBO started on {}", bind_address_clone))
}

#[defun]
fn stop(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();
    if let Some(State { runtime, .. }) = state.take() {
        runtime.shutdown_timeout(Duration::from_secs(5));
        env.message("HOBO stopped")
    } else {
        env.message("HOBO not running")
    }
}

async fn handle_websocket(socket: WebSocket, tx: broadcast::Sender<String>) {
    let (mut ws_sender, mut ws_receiver) = socket.split();
    let mut rx = tx.subscribe();
    let (internal_tx, mut internal_rx) = mpsc::channel::<String>(32);

    let ws_sender_task = tokio::spawn(async move {
        while let Some(msg) = internal_rx.recv().await {
            if ws_sender.send(Message::text(msg)).await.is_err() {
                break;
            }
        }
    });

    let broadcast_task = {
        let internal_tx = internal_tx.clone();
        tokio::spawn(async move {
            while let Ok(msg) = rx.recv().await {
                if internal_tx.send(msg).await.is_err() {
                    break;
                }
            }
        })
    };

    let receive_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = ws_receiver.next().await {
            todo!()
        }
    });

    tokio::select! {
        _ = ws_sender_task => {},
        _ = broadcast_task => {},
        _ = receive_task => {},
    }
}

#[defun]
fn handle_buffer_change(env: &Env, begin: u8, end: u8, length: u8) -> Result<()> {
    let state = STATE.lock().unwrap();

    if let Some(State { tx, .. }) = state.as_ref() {
        let content: String = buffer_string.call(env, [])?.into_rust()?;
        tx.send(content)?;
        Ok(())
    } else {
        Err(emacs::Error::msg("HOBO not started"))
    }
}
