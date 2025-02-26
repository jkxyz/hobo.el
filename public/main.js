const ws = new WebSocket("/ws");

ws.addEventListener("message", (event) => {
  output.innerText = event.data;
});
