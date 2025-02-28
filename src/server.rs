use axum::{
    extract::{ws::WebSocket, WebSocketUpgrade},
    routing::get,
    Router,
};
use tokio::net::TcpListener;
use tower_http::services::ServeDir;

pub async fn serve(bind_address: String, public_path: String) -> anyhow::Result<()> {
    let app: Router = Router::new()
        .route(
            "/socket",
            get(|upgrade: WebSocketUpgrade| async {
                upgrade.on_upgrade(|socket| handle_websocket(socket))
            }),
        )
        .fallback_service(ServeDir::new(public_path));

    let listener = TcpListener::bind(bind_address).await?;

    axum::serve(listener, app).await?;

    Ok(())
}

async fn handle_websocket(socket: WebSocket) {}
