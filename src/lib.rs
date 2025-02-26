use axum::{
    extract::ws::{Message, WebSocket, WebSocketUpgrade},
    Router,
};
use emacs::{defun, Env, Result, Value};
use futures_util::{sink::SinkExt, stream::StreamExt};
use std::{sync::Mutex, time::Duration};
use tokio::sync::{broadcast, mpsc};
use tokio::time::interval;
use tower_http::services::ServeDir;

emacs::use_symbols!(hobo_public_path hobo_server_bind_address symbol_value);

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

    let (tx, _rx) = broadcast::channel::<String>(100);
    let tx_clone = tx.clone();

    runtime.spawn(async move {
        let mut counter = 0;
        let mut interval = interval(Duration::from_secs(5));

        loop {
            interval.tick().await;
            let message = format!("Test broadcast message #{}", counter);
            let _ = tx_clone.send(message);
            counter += 1;
        }
    });

    let tx_for_router = tx.clone();

    runtime.spawn(async move {
        let app = Router::new()
            .route(
                "/ws",
                axum::routing::get(move |ws: WebSocketUpgrade| {
                    let tx = tx_for_router.clone();
                    async move { ws.on_upgrade(move |socket| handle_websocket(socket, tx)) }
                }),
            )
            .fallback_service(ServeDir::new(public_path));

        let listener = tokio::net::TcpListener::bind(&bind_address).await.unwrap();
        axum::serve(listener, app).await.unwrap();
    });

    *state = Some(State { runtime, tx });
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
