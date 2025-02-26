use std::sync::Mutex;

use axum::Router;
use emacs::{defun, Env, Result, Value};
use tower_http::services::ServeDir;

emacs::use_symbols!(hobo_public_path hobo_server_bind_address symbol_value);

emacs::plugin_is_GPL_compatible!();

struct State {
    runtime: tokio::runtime::Runtime,
}

static STATE: Mutex<Option<State>> = Mutex::new(None);

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

    let public_path: String = get_symbol_value(env, hobo_public_path)?;
    let bind_address: String = get_symbol_value(env, hobo_server_bind_address)?;
    let bind_address_clone = bind_address.clone();

    runtime.spawn(async {
        let app = Router::new().fallback_service(ServeDir::new(public_path));
        let listener = tokio::net::TcpListener::bind(bind_address).await.unwrap();
        axum::serve(listener, app).await.unwrap();
    });

    *state = Some(State { runtime });

    env.message(format!("HOBO started on {}", bind_address_clone))
}

#[defun]
fn stop(env: &Env) -> Result<Value<'_>> {
    let mut state = STATE.lock().unwrap();

    if let Some(State { runtime }) = state.take() {
        runtime.shutdown_timeout(std::time::Duration::from_secs(5));
    }

    env.message("HOBO stopped")
}

fn get_symbol_value<'a, T: emacs::FromLisp<'a>>(
    env: &'a Env,
    symbol: impl emacs::IntoLisp<'a>,
) -> Result<T> {
    symbol_value.call(env, (symbol,))?.into_rust()
}
