## Monpad

<div id="monpad-grid">

<div id="monpad-buttons">
  <button onclick='window.setMonpadLayout("default")'>
    default layout
  </button>
  <button onclick='window.setMonpadLayout("full")'>
    full gamepad layout
  </button>
  <button onclick='window.setMonpadLayout("mouse")'>
    mouse layout
  </button>
  <button onclick='window.sendMonpadUpdate({"ShowElement": "Red"})'>
    show red button
  </button>
  <button onclick='window.sendMonpadUpdate({"HideElement": "Red"})'>
    hide red button
  </button>
</div>

<iframe
  id="monpad"
  title="Monpad"
  width="400"
  height="200"
  src="/monpad.html?username=George"
>
</iframe>

<div class="wrapper">
<pre id="monpad-layout"></pre>
</div>

<div class="wrapper">
<pre id="monpad-output"></pre>
</div>

<script>
const maxLines = 10
const outputElement = document.getElementById("monpad-output")
const layoutElement = document.getElementById("monpad-layout")
document.addEventListener("monpad-client-update", e => {
  outputElement.textContent = outputElement.textContent.split("\n")
    .slice(-maxLines+1).join("\n")
    + (outputElement.textContent ? '\n' : '') + e.detail
})
window.sendMonpadUpdate = detail => document.dispatchEvent(new CustomEvent("monpad-server-update", {detail}))
window.setMonpadLayout = s => {
  window.fetch(`/portfolio/monpad/layouts/${s}.dhall`).then(r => r.text().then(t => {
    layoutElement.textContent = t
  }))
  // TODO eventually Monpad will have a Haskell Wasm frontend which will support Dhall input directly
  // then we can support actual freeform user input, and use `SetLayout` instead of `SwitchLayout`
  window.sendMonpadUpdate({"SwitchLayout": `${s}`})
}
</script>
