// Hand-written preload: the only bridge between the sandboxed renderer and the main process.
// Exposes a postMessage/onMessage pair over one IPC channel; nothing else crosses the boundary.
const { contextBridge, ipcRenderer } = require('electron')

const CHANNEL = 'morphir-rpc'

contextBridge.exposeInMainWorld('morphirIpc', {
  postMessage: (message) => ipcRenderer.send(CHANNEL, message),
  onMessage: (handler) => ipcRenderer.on(CHANNEL, (_event, message) => handler(message))
})
