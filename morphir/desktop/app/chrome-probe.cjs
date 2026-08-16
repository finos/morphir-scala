const { app, BrowserWindow } = require('electron')
const fs = require('fs')
const { start } = require('./dist/main.js')
start(__dirname)
app.whenReady().then(() => {
  setTimeout(async () => {
    try {
      const win = BrowserWindow.getAllWindows()[0]
      const hasSpacer = await win.webContents.executeJavaScript('!!document.getElementById("titlebar-drag")')
      const dragCss = await win.webContents.executeJavaScript('getComputedStyle(document.querySelector(".topbar")).webkitAppRegion')
      fs.writeFileSync('/tmp/chrome-probe.txt', `spacer=${hasSpacer} topbarDrag=${dragCss}\n`)
      fs.writeFileSync('/tmp/morphir-chrome.png', (await win.webContents.capturePage()).toPNG())
      win.focus()
    } catch (e) { fs.writeFileSync('/tmp/chrome-probe.txt', 'FAILED ' + e) }
  }, 6000)
})
