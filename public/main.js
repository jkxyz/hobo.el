import * as Y from "https://esm.run/yjs";

const socket = new WebSocket("/ws");

socket.addEventListener("message", (event) => {
  $output.innerText = event.data;
});
