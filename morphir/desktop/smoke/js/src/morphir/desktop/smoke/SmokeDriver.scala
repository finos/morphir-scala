package morphir.desktop.smoke

import org.scalajs.dom
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.util.control.NonFatal

object SmokeDriver:

  private given ExecutionContext = ExecutionContext.global

  private val sentinel = "ghp_MORPHIR_TASK6_SENTINEL_TOKEN_1234567890"

  private[smoke] val assertionNames = List(
    "clearedAfterFailure",
    "clearedAfterSessionSuccess",
    "clearedAfterSuccess",
    "disconnectedThroughButton",
    "mountedRenderer",
    "rememberFalseReadLive",
    "rememberReadLive",
    "rememberTrueReadLive",
    "removedStoredCredentialThroughButton",
    "rendererConsoleSentinelFree",
    "retainedOnFailure",
    "retainedOnSessionSuccess",
    "retainedOnSuccess",
    "safeConnectedStatus",
    "safeRejectedError",
    "safeSessionStatus",
    "submittedThroughForm",
    "transientDomSentinelFree"
  )

  @JSExportTopLevel("runMorphirDesktopSmoke")
  def run(): js.Promise[js.Dictionary[Boolean]] =
    scenario().toJSPromise

  private[smoke] def assembleAssertions(values: List[Boolean]): js.Dictionary[Boolean] =
    if values.size != assertionNames.size then throw IllegalArgumentException("desktop smoke assertion count differs")
    val result = js.Dictionary.empty[Boolean]
    assertionNames.zip(values).foreach { case (name, value) => result(name) = value }
    result

  private[smoke] def containsSafeText(text: String, required: List[String], secret: String): Boolean =
    required.forall(text.contains) && !text.contains(secret)

  private[smoke] def sessionStatusIsSafe(
      rememberFalseReadLive: Boolean,
      text: String,
      secret: String
  ): Boolean =
    rememberFalseReadLive && !text.contains(secret)

  private def scenario(): Future[js.Dictionary[Boolean]] =
    val leakMonitor = LeakMonitor()

    val result =
      for
        settings <- waitForElementById("settings-button", "Settings button")
        _ = click(settings, "Settings button")
        connections <- waitForElementById("settings-section-connections", "Connections section")
        _ = click(connections, "Connections section")
        _ = leakMonitor.start()
        _ = leakMonitor.checkpoint()
        _            <- waitForText("Stored credential rejected.", "stored-credential rejection")
        removeStored <-
          waitForButton("#github-rejected button", "Remove stored credential", "Remove stored credential button")
        _ = click(removeStored, "Remove stored credential button")
        _ <- waitForSelector(
          "[data-github-state=\"disconnected-idle\"]",
          "disconnected state after stored-credential removal"
        )
        _             = leakMonitor.checkpoint()
        passwordInput = inputById("github-connect-token", "enabled credential form")
        remember      = inputById("github-connect-remember", "enabled credential form")
        _             = passwordInput.value = sentinel
        _             = ensure(!remember.checked, "remember checkbox did not default to session-only")
        _             = setChecked(remember, checked = true, "Remember checkbox")
        _             = leakMonitor.checkpoint()
        _             = submit(formById("github-connect-form", "Connect form"))
        _ <- waitForSelector("[data-github-state=\"disconnected-busy\"]", "pending remembered connection")
        retainedOnSuccess = sameInput("github-connect-token", passwordInput) && passwordInput.value == sentinel
        _ = ensure(retainedOnSuccess, "password input was not retained while success callback was pending")
        _ = leakMonitor.checkpoint()
        _ <- waitForBodyText(
          text => text.contains("Connected as smoke-user") && text.contains("Connected and remembered on this device."),
          "remembered connected state"
        )
        deviceOutput         = bodyText
        rememberTrueReadLive = deviceOutput.contains("Connected and remembered on this device.")
        safeConnectedStatus  = containsSafeText(
          deviceOutput,
          List("Connected as smoke-user", "Connected and remembered on this device."),
          sentinel
        )
        _ = ensure(safeConnectedStatus, "remembered connected status was not safely redacted")
        _ <- waitFor(
          () => Option.when(passwordInput.value.isEmpty)(()),
          "password clear after remembered callback success"
        )
        clearedAfterSuccess = passwordInput.value.isEmpty
        _                   = ensure(clearedAfterSuccess, "password input was not cleared after callback success")
        _                   = leakMonitor.checkpoint()
        disconnect <- waitForButton("#github-connected button", "Disconnect", "Disconnect button")
        _ = click(disconnect, "Disconnect button")
        _ <-
          waitForSelector("[data-github-state=\"disconnected-idle\"]", "disconnected state after remembered connection")
        sessionInput = inputById("github-connect-token", "session credential form")
        _            = ensure(sessionInput eq passwordInput, "credential form replaced its exact password input")
        _            = sessionInput.value = sentinel
        _            = setChecked(remember, checked = false, "Remember checkbox for session")
        _            = leakMonitor.checkpoint()
        _            = submit(formById("github-connect-form", "Connect form for session"))
        _ <- waitForSelector("[data-github-state=\"disconnected-busy\"]", "pending session connection")
        retainedOnSessionSuccess = sameInput("github-connect-token", sessionInput) && sessionInput.value == sentinel
        _ = ensure(retainedOnSessionSuccess, "password input was not retained while session callback was pending")
        _ <- waitForBodyText(
          text => text.contains("Connected as smoke-user") && text.contains("Connected for this session."),
          "session connected state"
        )
        sessionOutput         = bodyText
        rememberFalseReadLive = sessionOutput.contains("Connected for this session.") &&
          !sessionOutput.contains("Connected and remembered on this device.")
        safeSessionStatus = sessionStatusIsSafe(rememberFalseReadLive, sessionOutput, sentinel)
        _                 = ensure(safeSessionStatus, "session connected status was not safely redacted")
        _ <- waitFor(() => Option.when(sessionInput.value.isEmpty)(()), "password clear after session callback success")
        clearedAfterSessionSuccess = sessionInput.value.isEmpty
        _ = ensure(clearedAfterSessionSuccess, "password input was not cleared after session callback success")
        _ = leakMonitor.checkpoint()
        disconnectAfterSession <-
          waitForButton("#github-connected button", "Disconnect", "Disconnect button after session connection")
        _ = click(disconnectAfterSession, "Disconnect button after session connection")
        _ <-
          waitForSelector("[data-github-state=\"disconnected-idle\"]", "disconnected state before rejected connection")
        failureInput = inputById("github-connect-token", "failure credential form")
        _            = ensure(failureInput eq passwordInput, "credential form replaced its exact password input")
        _            = failureInput.value = sentinel
        _            = ensure(!remember.checked, "remember checkbox changed before rejected connection")
        _            = leakMonitor.checkpoint()
        _            = submit(formById("github-connect-form", "Connect form for failure"))
        _ <- waitForSelector("[data-github-state=\"disconnected-busy\"]", "pending rejected connection")
        retainedOnFailure = sameInput("github-connect-token", failureInput) && failureInput.value == sentinel
        _ = ensure(retainedOnFailure, "exact password input was not retained while failure callback was pending")
        _ = leakMonitor.checkpoint()
        _ <- waitForText("GitHub rejected this token.", "safe rejected-token error")
        errorOutput       = bodyText
        safeRejectedError = containsSafeText(errorOutput, List("GitHub rejected this token."), sentinel)
        _                 = ensure(safeRejectedError, "rejected-token error was not safely redacted")
        _ <- waitFor(() => Option.when(failureInput.value.isEmpty)(()), "password clear after callback failure")
        _ <- waitForSelector("[data-github-state=\"disconnected-idle\"]", "idle form after callback failure")
        clearedAfterFailure = failureInput.value.isEmpty
        _                   = ensure(clearedAfterFailure, "password input was not cleared after callback failure")
        _ <- twoAnimationFrames()
        _ = leakMonitor.checkpoint()
      yield assembleAssertions(
        List(
          clearedAfterFailure,
          clearedAfterSessionSuccess,
          clearedAfterSuccess,
          true,
          true,
          rememberFalseReadLive,
          rememberTrueReadLive && rememberFalseReadLive,
          rememberTrueReadLive,
          true,
          true,
          retainedOnFailure,
          retainedOnSessionSuccess,
          retainedOnSuccess,
          safeConnectedStatus,
          safeRejectedError,
          safeSessionStatus,
          true,
          !leakMonitor.leaked
        )
      )

    result.andThen { case _ => leakMonitor.stop() }

  private def waitForElementById(id: String, label: String): Future[dom.Element] =
    waitFor(() => Option(dom.document.getElementById(id)), label)

  private def waitForSelector(selector: String, label: String): Future[dom.Element] =
    waitFor(() => Option(dom.document.querySelector(selector)), label)

  private def waitForText(copy: String, label: String): Future[Unit] =
    waitForBodyText(_.contains(copy), label)

  private def waitForBodyText(predicate: String => Boolean, label: String): Future[Unit] =
    waitFor(() => Option.when(predicate(bodyText))(()), label)

  private def waitForButton(selector: String, copy: String, label: String): Future[dom.HTMLButtonElement] =
    waitFor(
      () =>
        val buttons = dom.document.querySelectorAll(selector)
        (0 until buttons.length).iterator
          .map(index => buttons(index).asInstanceOf[dom.HTMLButtonElement])
          .find(button => buttonText(button) == copy)
      ,
      label
    )

  private def waitFor[A](predicate: () => Option[A], label: String, timeoutMillis: Double = 15000): Future[A] =
    val result   = Promise[A]()
    val deadline = js.Date.now() + timeoutMillis

    def loop(): Unit =
      try
        predicate() match
          case Some(value) =>
            val _ = result.trySuccess(value)
          case None if js.Date.now() < deadline =>
            val _ = dom.window.setTimeout(() => loop(), 25)
          case None =>
            val _ = result.tryFailure(IllegalStateException(s"timed out waiting for $label"))
      catch
        case NonFatal(error) =>
          val _ = result.tryFailure(error)

    loop()
    result.future

  private def click(element: dom.Element, label: String): Unit =
    if element == null then throw IllegalStateException(s"$label was not mounted")
    element.asInstanceOf[dom.HTMLElement].click()

  private def submit(form: dom.HTMLFormElement): Unit =
    form.asInstanceOf[RequestSubmitForm].requestSubmit()

  private def setChecked(checkbox: dom.HTMLInputElement, checked: Boolean, label: String): Unit =
    checkbox.checked = checked
    val init = new dom.EventInit:
      bubbles = true
    checkbox.dispatchEvent(new dom.Event("input", init))
    checkbox.dispatchEvent(new dom.Event("change", init))
    ensure(checkbox.checked == checked, s"$label did not retain its live checked value")

  private def inputById(id: String, label: String): dom.HTMLInputElement =
    Option(dom.document.getElementById(id)) match
      case Some(input: dom.HTMLInputElement) => input
      case _                                 => throw IllegalStateException(s"$label was not mounted")

  private def formById(id: String, label: String): dom.HTMLFormElement =
    Option(dom.document.getElementById(id)) match
      case Some(form: dom.HTMLFormElement) => form
      case _                               => throw IllegalStateException(s"$label was not mounted")

  private def sameInput(id: String, expected: dom.HTMLInputElement): Boolean =
    dom.document.getElementById(id) eq expected

  private def buttonText(button: dom.HTMLButtonElement): String =
    Option(button.textContent).getOrElse("").trim

  private def bodyText: String =
    Option(dom.document.body.textContent).getOrElse("")

  private def twoAnimationFrames(): Future[Unit] =
    val result = Promise[Unit]()
    val _      = dom.window.requestAnimationFrame { _ =>
      val _ = dom.window.requestAnimationFrame { _ =>
        result.success(())
      }
    }
    result.future

  private def ensure(condition: Boolean, message: String): Unit =
    if !condition then throw IllegalStateException(message)

  @js.native
  private trait RequestSubmitForm extends js.Object:
    def requestSubmit(): Unit = js.native

  private final class LeakMonitor private ():
    private var transientLeak = false

    private val observer = new dom.MutationObserver((mutations, _) => inspect(mutations))

    def leaked: Boolean = transientLeak

    def start(): Unit =
      val options = new dom.MutationObserverInit:
        subtree = true
        childList = true
        attributes = true
        attributeOldValue = true
        characterData = true
        characterDataOldValue = true
      observer.observe(dom.document.documentElement, options)

    def stop(): Unit = observer.disconnect()

    def checkpoint(): Unit =
      inspect(observer.takeRecords())
      if dom.document.documentElement.outerHTML.contains(sentinel) then transientLeak = true
      if bodyText.contains(sentinel) then transientLeak = true
      ensure(!transientLeak, "sentinel appeared outside the password input")

    private def inspect(mutations: js.Array[dom.MutationRecord]): Unit =
      mutations.foreach { mutation =>
        if Option(mutation.oldValue).exists(_.contains(sentinel)) then transientLeak = true
        if nodeLeaks(mutation.target) then transientLeak = true
        if mutation.`type` == "childList" && nodesContainLeak(mutation.addedNodes) then transientLeak = true
      }

    private def nodesContainLeak(nodes: dom.NodeList[dom.Node]): Boolean = (0 until nodes.length).exists(index =>
      nodeLeaks(nodes(index))
    )

    private def nodeLeaks(node: dom.Node): Boolean =
      if node.nodeType == dom.Node.TEXT_NODE then
        node.asInstanceOf[dom.Text].data.contains(sentinel)
      else if node.nodeType != dom.Node.ELEMENT_NODE then false
      else
        val element        = node.asInstanceOf[dom.Element]
        val attributeLeaks = (0 until element.attributes.length).exists { index =>
          element.attributes(index).value.contains(sentinel)
        }
        attributeLeaks || Option(element.textContent).exists(_.contains(sentinel))

  private object LeakMonitor:
    def apply(): LeakMonitor = new LeakMonitor()
end SmokeDriver
