## Monpad

<iframe
  id="monpad"
  title="Monpad"
  width="400"
  height="200"
  src="monpad?username=George"
>
</iframe>

<pre id="monpad-output"></pre>

<script>
const maxLines = 10
const outputElement = document.getElementById("monpad-output")
document.addEventListener("monpad-client-update", e => {
  outputElement.textContent = outputElement.textContent.split("\n")
    .slice(-maxLines+1).join("\n")
    + (outputElement.textContent ? '\n' : '') + e.detail
})
</script>
