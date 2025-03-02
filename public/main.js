import * as Y from "https://esm.run/yjs";
import { TextAreaBinding } from "https://esm.run/y-textarea";

const doc = new Y.Doc();

const buffer = doc.getText("*hobo*");

const binding = new TextAreaBinding(buffer, $output);

window.DOC = doc;

const socket = new WebSocket("/socket");

socket.addEventListener("open", (_) => {});

socket.addEventListener("message", (event) => {
  const message = JSON.parse(event.data);

  if (message.type === "Diff") {
    Y.applyUpdate(doc, new Uint8Array(message.diff));
  }
});

doc.on("update", (diff, origin) => {
  socket.send(JSON.stringify({ type: "Diff", diff: Array.from(diff) }));
});
