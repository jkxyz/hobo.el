use std::sync::Mutex;

use axum::{routing::get, Router};
use emacs::{defun, Env, Result, Value};

emacs::plugin_is_GPL_compatible!();

struct State {
    runtime: tokio::runtime::Runtime,
}

static STATE: Mutex<Option<State>> = Mutex::new(None);

#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("HOBO module loaded")
}

#[defun]
fn start(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();

    if state.is_some() {
        return env.message("HOBO already started");
    }

    let runtime = tokio::runtime::Runtime::new().unwrap();

    runtime.spawn(async {
        let app = Router::new().route("/", get(|| async { "Hello, World!" }));
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
