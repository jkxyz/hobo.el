use std::{
    future::IntoFuture,
    sync::OnceLock,
    time::{Duration, Instant},
};

use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing, Router,
};
use emacs::IntoLisp;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;

emacs::plugin_is_GPL_compatible!();

emacs::use_symbols!(hobo_public_path
                    hobo_server_bind_address
                    hobo_display_error
                    symbol_value);

struct HoboState {
    runtime: tokio::runtime::Runtime,
    errors_tx: tokio::sync::mpsc::Sender<anyhow::Error>,
    errors_rx: tokio::sync::Mutex<tokio::sync::mpsc::Receiver<anyhow::Error>>,
    server_task: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
}

static STATE: OnceLock<HoboState> = OnceLock::new();

#[emacs::module(name = "hobors", separator = "--")]
fn init(_env: &emacs::Env) -> emacs::Result<()> {
    let runtime = tokio::runtime::Runtime::new()?;

    let (errors_tx, errors_rx) = tokio::sync::mpsc::channel::<anyhow::Error>(100);

    STATE
        .set(HoboState {
            runtime,
            errors_tx,
            errors_rx: tokio::sync::Mutex::new(errors_rx),
            server_task: tokio::sync::Mutex::new(None),
        })
        .map_err(|_| anyhow::anyhow!("Failed to initialize global state: already initialized"))?;

    Ok(())
}

#[emacs::defun]
fn start(env: &emacs::Env) -> emacs::Result<()> {
    let HoboState {
        runtime,
        errors_tx,
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
                errors_tx.send(anyhow::anyhow!(err)).await.unwrap();
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
fn get_new_errors(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let HoboState {
        runtime, errors_rx, ..
    } = get_state()?;

    let mut errors_rx = runtime.block_on(errors_rx.lock());

    let deadline = Instant::now() + Duration::from_millis(100);
    let mut errors = Vec::<anyhow::Error>::new();

    while Instant::now() < deadline {
        match errors_rx.try_recv() {
            Ok(err) => {
                errors.push(err);
            }
            Err(tokio::sync::mpsc::error::TryRecvError::Empty) => break,
            Err(tokio::sync::mpsc::error::TryRecvError::Disconnected) => {
                errors.push(anyhow::anyhow!("Errors channel closed unexpectedly"));
            }
        }
    }

    let error_lisp_strings = errors
        .into_iter()
        .map(|err| err.to_string().into_lisp(env))
        .collect::<emacs::Result<Vec<emacs::Value<'_>>>>()?;

    env.list(&error_lisp_strings)
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

async fn handle_websocket(socket: WebSocket) {}

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
