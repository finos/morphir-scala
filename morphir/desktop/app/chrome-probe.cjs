const { app, BrowserWindow } = require('electron')
const fs = require('fs')
const { start } = require('./dist/main.js')
start(__dirname)
app.whenReady().then(() => {
  setTimeout(async () => {
    try {
      const win = BrowserWindow.getAllWindows()[0]
      fs.writeFileSync('/tmp/chrome-expanded.png', (await win.webContents.capturePage()).toPNG())
      await win.webContents.executeJavaScript('document.getElementById("sidebar-toggle").click()')
      await new Promise(r => setTimeout(r, 600))
      fs.writeFileSync('/tmp/chrome-collapsed.png', (await win.webContents.capturePage()).toPNG())
      const back = await win.webContents.executeJavaScript('document.getElementById("sidebar-toggle").click(); "ok"')
      await new Promise(r => setTimeout(r, 600))
      const cls = await win.webContents.executeJavaScript('document.querySelectorAll(".sidebar").length')
      fs.writeFileSync('/tmp/chrome-probe.txt', `roundtrip sidebars=${cls}\n`)
      win.focus()
    } catch (e) { fs.writeFileSync('/tmp/chrome-probe.txt', 'FAILED ' + e) }
  }, 6000)
})
