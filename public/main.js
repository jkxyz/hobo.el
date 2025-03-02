import * as Y from "https://cdn.skypack.dev/yjs";
import { TextAreaBinding } from "https://cdn.skypack.dev/y-textarea";

const doc = new Y.Doc();

const buffer = doc.getText("*hobo*");

const binding = new TextAreaBinding(buffer, $output);

window.DOC = doc;

const socket = new WebSocket("/socket");

socket.addEventListener("open", (_) => {});

socket.addEventListener("message", (event) => {
  const message = JSON.parse(event.data);

  if (message.type === "Diff") {
    Y.applyUpdate(doc, new Uint8Array(message.diff), "server");
  }
});

doc.on("update", (diff, origin) => {
  if (origin !== "server") {
    socket.send(JSON.stringify({ type: "Diff", diff: Array.from(diff) }));
  }
});
