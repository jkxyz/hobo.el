import * as Y from "https://esm.run/yjs";

const doc = new Y.Doc();

window.DOC = doc;

const socket = new WebSocket("/socket");

socket.addEventListener("open", (_) => {});

socket.addEventListener("message", (event) => {
  const message = JSON.parse(event.data);

  if (message.type === "DIFF") {
    Y.applyUpdate(doc, new Uint8Array(message.diff));
    $output.innerText = doc.getText("*hobo*").toString();
  }
});
