use std::sync::OnceLock;

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

emacs::define_errors! {
    hobo_error "Hobo error"
}

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

    runtime.block_on(async {
        if let Some(server_task) = server_task.lock().await.take() {
            server_task.abort();
        }
    });

    env.message("Hobo stopped")?;

    Ok(())
}

#[emacs::defun]
fn last_error(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let HoboState {
        runtime, errors_rx, ..
    } = get_state()?;

    let mut errors_rx = runtime.block_on(errors_rx.lock());

    match errors_rx.try_recv() {
        Ok(err) => Ok(format!("{}", err).into_lisp(env)?),
        Err(tokio::sync::mpsc::error::TryRecvError::Empty) => Ok(false.into_lisp(env)?),
        Err(tokio::sync::mpsc::error::TryRecvError::Disconnected) => {
            Ok("Errors channel closed".into_lisp(env)?)
        }
    }
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
