const fs = require('fs')
const { app, BrowserWindow } = require('electron')
const { startSmoke } = require('./dist/main.js')

const required = (name) => {
  const value = process.env[name]
  if (!value) throw new Error(`missing ${name}`)
  return value
}

const userDataPath = required('MORPHIR_DESKTOP_SMOKE_USER_DATA')
const screenshotPath = required('MORPHIR_DESKTOP_SMOKE_SCREENSHOT')
const resultPath = required('MORPHIR_DESKTOP_SMOKE_RESULT')
const rendererLogPath = required('MORPHIR_DESKTOP_SMOKE_RENDERER_LOG')
const sentinel = required('MORPHIR_DESKTOP_SMOKE_SENTINEL')
const rendererLines = []

const consoleMessage = (args) => {
  const details = args[1]
  if (details && typeof details === 'object' && typeof details.message === 'string') return details.message
  if (typeof args[2] === 'string') return args[2]
  if (typeof args[1] === 'string') return args[1]
  return ''
}

const closeWindow = async (win) => {
  if (win.isDestroyed()) return
  await new Promise((resolve) => {
    win.once('closed', resolve)
    win.close()
  })
}

const windowReady = new Promise((resolve, reject) => {
  app.once('browser-window-created', (_event, win) => {
    win.once('closed', () => reject(new Error('desktop window closed before loading')))
    win.webContents.once('did-fail-load', (_loadEvent, code) => {
      reject(new Error(`desktop page failed to load (${code})`))
    })
    win.webContents.once('did-finish-load', () => resolve(win))
  })
})

const writeRendererLog = () => {
  fs.writeFileSync(rendererLogPath, rendererLines.length === 0 ? '' : `${rendererLines.join('\n')}\n`)
}

app.setPath('userData', userDataPath)
app.on('window-all-closed', () => {})
app.on('web-contents-created', (_event, contents) => {
  contents.on('console-message', (...args) => rendererLines.push(consoleMessage(args)))
})
startSmoke(__dirname)

app.whenReady().then(async () => {
  try {
    const win = await windowReady
    const source = fs.readFileSync('./dist/smoke-driver.js', 'utf8')
    const result = await win.webContents.executeJavaScript(`(() => {
      const exports = {}
      ${source}
      return exports.runMorphirDesktopSmoke()
    })()`)
    const image = await win.webContents.capturePage()
    fs.writeFileSync(screenshotPath, image.toPNG())
    await closeWindow(win)
    writeRendererLog()
    fs.writeFileSync(resultPath, `${JSON.stringify({
      ...result,
      rendererConsoleSentinelFree: rendererLines.every((message) => !message.includes(sentinel))
    }, null, 2)}\n`)
    app.exit(0)
  } catch (_) {
    await Promise.all(BrowserWindow.getAllWindows().map(closeWindow))
    writeRendererLog()
    console.error('SMOKE FAILED')
    app.exit(1)
  }
})
