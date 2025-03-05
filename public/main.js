import * as Y from "https://cdn.skypack.dev/yjs";
import { TextAreaBinding } from "https://cdn.skypack.dev/y-textarea";

const doc = new Y.Doc();

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

  render();
});

const bindings = [];

function render() {
  const buffers = doc.getArray("buffers").toArray();

  for (const bufferName of buffers) {
    if (getBufferEl(bufferName)) continue;

    const bufferEl = $bufferTemplate.content.cloneNode(true);

    bufferEl.querySelector(".buffer").dataset.buffer = bufferName;
    bufferEl.querySelector(".buffer-title").innerText = bufferName;

    $root.appendChild(bufferEl);

    bindings.push(
      new TextAreaBinding(
        doc.getText(bufferName),
        getBufferEl(bufferName).querySelector(".buffer-content"),
      ),
    );
  }

  const buffersSet = new Set(buffers);

  for (const bufferEl of $root.querySelectorAll("[data-buffer]")) {
    if (!buffersSet.has(bufferEl.dataset.buffer)) {
      bufferEl.remove();
    }
  }
}

function getBufferEl(bufferName) {
  return $root.querySelector(`[data-buffer="${bufferName}"]`);
}
