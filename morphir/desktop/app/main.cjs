// Electron entry: the Scala.js main-process bundle (CommonJS) exports start(appDir).
const { start } = require('./dist/main.js')
start(__dirname)
