// Smoke driver: boots the assembled app, exercises the mounted renderer through Chromium DOM events,
// and writes only redacted assertion results plus capture artifacts to /tmp.
const fs = require('fs')
const path = require('path')
const { app, BrowserWindow } = require('electron')
const { startSmoke } = require('./dist/main.js')

const screenshotPath = '/tmp/morphir-desktop-smoke.png'
const resultPath = '/tmp/morphir-desktop-smoke.json'
const consolePath = '/tmp/morphir-desktop-smoke-renderer-console.log'
const sentinel = 'ghp_MORPHIR_TASK6_SENTINEL_TOKEN_1234567890'
const userDataPath = process.argv[2]
const teardownSentinelFixture = process.argv.includes('--teardown-console-sentinel')
if (!userDataPath || !path.isAbsolute(userDataPath)) {
  throw new Error('smoke harness must supply an absolute isolated userData directory')
}
const rendererConsole = []

const safeMessage = (value) => String(value).split(sentinel).join('<redacted>')

const consoleMessage = (args) => {
  const details = args[1]
  if (details && typeof details === 'object' && typeof details.message === 'string') return details.message
  if (typeof args[2] === 'string') return args[2]
  if (typeof args[1] === 'string') return args[1]
  return ''
}

const writeRendererConsole = () => {
  const content = rendererConsole.length === 0 ? '' : rendererConsole.join('\n') + '\n'
  fs.writeFileSync(consolePath, content)
}

const closeWindow = async (win) => {
  if (win.isDestroyed()) return
  await new Promise((resolve) => {
    win.once('closed', resolve)
    win.close()
  })
  if (!win.isDestroyed()) throw new Error('desktop window did not reach its destroyed state')
}

app.setPath('userData', userDataPath)
// Keep the smoke main process alive while teardown artifacts are finalized after the renderer closes.
app.on('window-all-closed', () => {})
app.on('web-contents-created', (_event, contents) => {
  contents.on('console-message', (...args) => {
    rendererConsole.push(consoleMessage(args))
  })
  if (teardownSentinelFixture) {
    contents.once('destroyed', () => {
      rendererConsole.push(sentinel)
      fs.writeFileSync(path.join(userDataPath, 'teardown-sentinel.txt'), sentinel)
    })
  }
})
startSmoke(__dirname)

const waitForWindow = async () => {
  const deadline = Date.now() + 15000
  while (Date.now() < deadline) {
    const win = BrowserWindow.getAllWindows()[0]
    if (win) return win
    await new Promise((resolve) => setTimeout(resolve, 25))
  }
  throw new Error('desktop window did not open')
}

app.whenReady().then(async () => {
  try {
    const win = await waitForWindow()
    const result = await win.webContents.executeJavaScript(`(async () => {
      const sentinel = ${JSON.stringify(sentinel)}
      const waitFor = async (predicate, label, timeout = 15000) => {
        const deadline = Date.now() + timeout
        while (Date.now() < deadline) {
          const value = predicate()
          if (value) return value
          await new Promise((resolve) => setTimeout(resolve, 25))
        }
        throw new Error('timed out waiting for ' + label)
      }
      const click = (element, label) => {
        if (!element) throw new Error(label + ' was not mounted')
        element.click()
      }
      const submit = (form, label) => {
        if (!form) throw new Error(label + ' was not mounted')
        form.requestSubmit()
      }
      const setChecked = (checkbox, checked, label) => {
        if (!checkbox) throw new Error(label + ' was not mounted')
        checkbox.checked = checked
        checkbox.dispatchEvent(new Event('input', { bubbles: true }))
        checkbox.dispatchEvent(new Event('change', { bubbles: true }))
        if (checkbox.checked !== checked) throw new Error(label + ' did not retain its live checked value')
      }
      const nodeLeaks = (node) => {
        if (node.nodeType === Node.TEXT_NODE) return node.data.includes(sentinel)
        if (node.nodeType !== Node.ELEMENT_NODE) return false
        if (Array.from(node.attributes).some((attribute) => attribute.value.includes(sentinel))) return true
        return node.textContent.includes(sentinel)
      }
      let transientDomLeak = false
      const inspectMutations = (mutations) => {
        for (const mutation of mutations) {
          if (mutation.oldValue && mutation.oldValue.includes(sentinel)) transientDomLeak = true
          if (nodeLeaks(mutation.target)) transientDomLeak = true
          if (mutation.type === 'childList' && Array.from(mutation.addedNodes).some(nodeLeaks)) transientDomLeak = true
        }
      }
      const observer = new MutationObserver(inspectMutations)
      const domCheckpoint = () => {
        inspectMutations(observer.takeRecords())
        if (document.documentElement.outerHTML.includes(sentinel)) transientDomLeak = true
        if (document.body.textContent.includes(sentinel)) transientDomLeak = true
        if (transientDomLeak) throw new Error('sentinel appeared outside the password input')
      }

      click(await waitFor(() => document.getElementById('settings-button'), 'Settings button'), 'Settings button')
      click(
        await waitFor(() => document.getElementById('settings-section-connections'), 'Connections section'),
        'Connections section'
      )
      observer.observe(document.documentElement, {
        subtree: true,
        childList: true,
        attributes: true,
        attributeOldValue: true,
        characterData: true,
        characterDataOldValue: true
      })
      domCheckpoint()

      await waitFor(
        () => document.body.textContent.includes('Stored credential rejected.'),
        'stored-credential rejection'
      )
      const removeStored = Array.from(document.querySelectorAll('#github-rejected button'))
        .find((button) => button.textContent.trim() === 'Remove stored credential')
      click(removeStored, 'Remove stored credential button')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-idle"]'),
        'disconnected state after stored-credential removal'
      )
      domCheckpoint()

      const passwordInput = document.getElementById('github-connect-token')
      const remember = document.getElementById('github-connect-remember')
      if (!passwordInput || !remember) throw new Error('enabled credential form was not mounted')
      passwordInput.value = sentinel
      if (remember.checked) throw new Error('remember checkbox did not default to session-only')
      setChecked(remember, true, 'Remember checkbox')
      domCheckpoint()
      submit(document.getElementById('github-connect-form'), 'Connect form')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-busy"]'),
        'pending remembered connection'
      )
      const retainedOnSuccess = document.getElementById('github-connect-token') === passwordInput &&
        passwordInput.value === sentinel
      if (!retainedOnSuccess) throw new Error('password input was not retained while success callback was pending')
      domCheckpoint()

      await waitFor(
        () => document.body.textContent.includes('Connected as smoke-user') &&
          document.body.textContent.includes('Connected and remembered on this device.'),
        'remembered connected state'
      )
      const deviceOutput = document.body.textContent
      const rememberTrueReadLive = deviceOutput.includes('Connected and remembered on this device.')
      const safeConnectedStatus = deviceOutput.includes('Connected as smoke-user') &&
        rememberTrueReadLive && !deviceOutput.includes(sentinel)
      if (!safeConnectedStatus) throw new Error('remembered connected status was not safely redacted')
      await waitFor(() => passwordInput.value === '', 'password clear after remembered callback success')
      const clearedAfterSuccess = passwordInput.value === ''
      if (!clearedAfterSuccess) throw new Error('password input was not cleared after callback success')
      domCheckpoint()

      const disconnect = () => Array.from(document.querySelectorAll('#github-connected button'))
        .find((button) => button.textContent.trim() === 'Disconnect')
      click(disconnect(), 'Disconnect button')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-idle"]'),
        'disconnected state after remembered connection'
      )

      const sessionInput = document.getElementById('github-connect-token')
      if (sessionInput !== passwordInput) throw new Error('credential form replaced its exact password input')
      sessionInput.value = sentinel
      setChecked(remember, false, 'Remember checkbox for session')
      domCheckpoint()
      submit(document.getElementById('github-connect-form'), 'Connect form for session')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-busy"]'),
        'pending session connection'
      )
      const retainedOnSessionSuccess = document.getElementById('github-connect-token') === sessionInput &&
        sessionInput.value === sentinel
      if (!retainedOnSessionSuccess) throw new Error('password input was not retained while session callback was pending')

      await waitFor(
        () => document.body.textContent.includes('Connected as smoke-user') &&
          document.body.textContent.includes('Connected for this session.'),
        'session connected state'
      )
      const sessionOutput = document.body.textContent
      const rememberFalseReadLive = sessionOutput.includes('Connected for this session.') &&
        !sessionOutput.includes('Connected and remembered on this device.')
      const safeSessionStatus = rememberFalseReadLive && !sessionOutput.includes(sentinel)
      if (!safeSessionStatus) throw new Error('session connected status was not safely redacted')
      await waitFor(() => sessionInput.value === '', 'password clear after session callback success')
      const clearedAfterSessionSuccess = sessionInput.value === ''
      if (!clearedAfterSessionSuccess) throw new Error('password input was not cleared after session callback success')
      domCheckpoint()

      click(disconnect(), 'Disconnect button after session connection')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-idle"]'),
        'disconnected state before rejected connection'
      )

      const failureInput = document.getElementById('github-connect-token')
      if (failureInput !== passwordInput) throw new Error('credential form replaced its exact password input')
      failureInput.value = sentinel
      if (remember.checked) throw new Error('remember checkbox changed before rejected connection')
      domCheckpoint()
      submit(document.getElementById('github-connect-form'), 'Connect form for failure')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-busy"]'),
        'pending rejected connection'
      )
      const retainedOnFailure = document.getElementById('github-connect-token') === failureInput &&
        failureInput.value === sentinel
      if (!retainedOnFailure) throw new Error('exact password input was not retained while failure callback was pending')
      domCheckpoint()

      await waitFor(
        () => document.body.textContent.includes('GitHub rejected this token.'),
        'safe rejected-token error'
      )
      const errorOutput = document.body.textContent
      const safeRejectedError = errorOutput.includes('GitHub rejected this token.') &&
        !errorOutput.includes(sentinel)
      if (!safeRejectedError) throw new Error('rejected-token error was not safely redacted')
      await waitFor(() => failureInput.value === '', 'password clear after callback failure')
      await waitFor(
        () => document.querySelector('[data-github-state="disconnected-idle"]'),
        'idle form after callback failure'
      )
      const clearedAfterFailure = failureInput.value === ''
      if (!clearedAfterFailure) throw new Error('password input was not cleared after callback failure')
      await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)))
      domCheckpoint()
      observer.disconnect()

      return {
        mountedRenderer: true,
        submittedThroughForm: true,
        rememberReadLive: rememberTrueReadLive && rememberFalseReadLive,
        rememberTrueReadLive,
        rememberFalseReadLive,
        retainedOnSuccess,
        clearedAfterSuccess,
        retainedOnSessionSuccess,
        clearedAfterSessionSuccess,
        disconnectedThroughButton: true,
        removedStoredCredentialThroughButton: true,
        retainedOnFailure,
        clearedAfterFailure,
        safeConnectedStatus,
        safeSessionStatus,
        safeRejectedError,
        transientDomSentinelFree: !transientDomLeak
      }
    })()`)

    const image = await win.webContents.capturePage()
    fs.writeFileSync(screenshotPath, image.toPNG())
    await closeWindow(win)
    writeRendererConsole()
    const rendererConsoleSentinelFree = rendererConsole.every((message) => !message.includes(sentinel))
    const completeResult = { ...result, rendererConsoleSentinelFree }
    fs.writeFileSync(resultPath, JSON.stringify(completeResult, null, 2) + '\n')
    if (!rendererConsoleSentinelFree) throw new Error('sentinel appeared in renderer console output')
    app.exit(0)
  } catch (error) {
    await Promise.all(BrowserWindow.getAllWindows().map(closeWindow))
    try {
      writeRendererConsole()
    } catch (_) {}
    console.error('SMOKE FAILED:', safeMessage(error instanceof Error ? error.message : 'unknown error'))
    app.exit(1)
  }
})
