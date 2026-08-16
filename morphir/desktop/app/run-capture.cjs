// Boot the real app visibly, capture proof-of-window after load, keep running.
const { app, BrowserWindow } = require('electron')
const { start } = require('./dist/main.js')
start(__dirname)
app.whenReady().then(() => {
  setTimeout(async () => {
    try {
      const win = BrowserWindow.getAllWindows()[0]
      const img = await win.webContents.capturePage()
      require('fs').writeFileSync('/tmp/morphir-desktop-live.png', img.toPNG())
      win.focus()
    } catch (e) { console.error('capture failed', e) }
  }, 6000)
})
