import * as Y from "https://cdn.skypack.dev/yjs";

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

function render() {
  for (const bufferName in doc.toJSON()) {
    let bufferEl = $root.querySelector(`[data-buffer="${bufferName}"]`);

    if (!bufferEl) {
      bufferEl = $bufferTemplate.content.cloneNode(true);
      bufferEl.querySelector(".buffer").dataset.buffer = bufferName;
      bufferEl.querySelector(".buffer-title").innerText = bufferName;

      $root.appendChild(bufferEl);

      bufferEl = $root.querySelector(`[data-buffer="${bufferName}"]`);
    }

    bufferEl.querySelector(".buffer-content").innerText = doc
      .getText(bufferName)
      .toString();
  }
}
