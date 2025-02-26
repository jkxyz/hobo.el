use std::sync::Mutex;

use axum::Router;
use emacs::{defun, Env, Result, Value};
use tower_http::services::ServeDir;

emacs::plugin_is_GPL_compatible!();

struct State {
    runtime: tokio::runtime::Runtime,
}

static STATE: Mutex<Option<State>> = Mutex::new(None);

// TODO Make this a custom variable in Emacs
static PUBLIC_PATH: &str = "C:/Users/josh/Code/hobo/public";

#[emacs::module(name = "hobors")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

#[defun]
fn start(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();

    if state.is_some() {
        return env.message("HOBO already started");
    }

    let runtime = tokio::runtime::Runtime::new().unwrap();

    runtime.spawn(async {
        let app = Router::new().fallback_service(ServeDir::new(PUBLIC_PATH));

        let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();

        axum::serve(listener, app).await.unwrap();
    });

    *state = Some(State { runtime });

    env.message("HOBO started on localhost:3000")
}

#[defun]
fn stop(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();

    if let Some(State { runtime }) = state.take() {
        runtime.shutdown_timeout(std::time::Duration::from_secs(5));
    }

    env.message("HOBO stopped")
}
