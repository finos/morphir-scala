const { app, BrowserWindow } = require('electron')
const fs = require('fs')
const { start } = require('./dist/main.js')
start(__dirname)
app.whenReady().then(() => {
  setTimeout(async () => {
    try {
      const win = BrowserWindow.getAllWindows()[0]
      const js = (code) => win.webContents.executeJavaScript(code)
      fs.writeFileSync('/tmp/regions-all.png', (await win.webContents.capturePage()).toPNG())
      await js('document.getElementById("right-toggle").click()')
      await js('document.getElementById("bottom-toggle").click()')
      await js('document.getElementById("sidebar-toggle").click()')
      await new Promise(r => setTimeout(r, 700))
      fs.writeFileSync('/tmp/regions-none.png', (await win.webContents.capturePage()).toPNG())
      const counts = await js('[document.querySelectorAll(".sidebar").length, document.querySelectorAll(".rightbar").length, document.querySelectorAll(".bottombar").length].join(",")')
      await js('document.getElementById("right-toggle").click(); document.getElementById("bottom-toggle").click(); document.getElementById("sidebar-toggle").click()')
      await new Promise(r => setTimeout(r, 700))
      const counts2 = await js('[document.querySelectorAll(".sidebar").length, document.querySelectorAll(".rightbar").length, document.querySelectorAll(".bottombar").length].join(",")')
      fs.writeFileSync('/tmp/chrome-probe.txt', `collapsed: ${counts}\nrestored: ${counts2}\n`)
      win.focus()
    } catch (e) { fs.writeFileSync('/tmp/chrome-probe.txt', 'FAILED ' + e) }
  }, 6000)
})
