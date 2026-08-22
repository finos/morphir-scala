package morphir.ui.github

import kyo.*
import kyo.UI.*
import morphir.ui.SettingsView
import morphir.ui.services.*
import morphir.ui.theme.Tokens

private[github] trait GitHubConnectionSubmission:
  def tokenValue: String < Sync
  def rememberChecked: Boolean < Sync
  def clearToken: Unit < Sync

private[github] trait GitHubConnectionForm:
  def capture(target: GitHubConnectionForm.Target): GitHubConnectionSubmission < Sync

private[github] object GitHubConnectionForm:
  enum Target(val formId: String, val tokenInputId: String, val rememberInputId: String):
    case Connect
        extends Target(
          "github-connect-form",
          "github-connect-token",
          "github-connect-remember"
        )
    case Replace
        extends Target(
          "github-replace-form",
          "github-replace-token",
          "github-replace-remember"
        )

object GitHubConnectionView:

  object Css:
    val panel      = "github-connection"
    val status     = "github-connection-status"
    val detail     = "github-connection-detail"
    val section    = "github-connection-section"
    val form       = "github-connection-form"
    val tokenLabel = "github-token-label"
    val token      = "github-token-input"
    val remember   = "github-remember"
    val actions    = "github-connection-actions"
    val action     = "github-connection-action"
    val secondary  = "github-connection-secondary"
    val progress   = "github-connection-progress"
    val error      = "github-connection-error"

  def view(
      state: Signal[GitHubConnectionStore.State],
      onConnect: (TokenSubmission, Boolean) => Any < Async,
      onDisconnect: => Any < Async
  ): UI =
    viewWithForm(state, GitHubConnectionDom.form, onConnect, onDisconnect)

  private[github] def viewWithForm(
      state: Signal[GitHubConnectionStore.State],
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async,
      onDisconnect: => Any < Async
  ): UI =
    SettingsView.connections(
      panel(state, fields, onConnect, onDisconnect)
    )

  private[github] def submit(
      target: GitHubConnectionForm.Target,
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async
  ): Any < Async =
    fields.capture(target).map { submission =>
      Scope.run {
        Scope.ensure(submission.clearToken).andThen {
          for
            raw      <- submission.tokenValue
            remember <- submission.rememberChecked
            result   <- onConnect(TokenSubmission.from(raw), remember)
          yield result
        }
      }
    }

  private def panel(
      state: Signal[GitHubConnectionStore.State],
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async,
      onDisconnect: => Any < Async
  ): UI =
    div.cssClass(Css.panel).id("github-connection")(
      div.cssClass(Css.status)("GitHub.com"),
      state.render(presentation),
      disconnected(fields, onConnect),
      connected(onDisconnect),
      rejected(fields, onConnect, onDisconnect)
    )

  private def presentation(state: GitHubConnectionStore.State): UI =
    fragment(
      visibility(state),
      state.safeError.fold[UI](UI.empty)(message => div.cssClass(Css.error).role("alert")(message)),
      state.status match
        case GitHubConnectionStatus.Disconnected                  => UI.empty
        case GitHubConnectionStatus.Connected(login, persistence) =>
          val persistenceCopy = persistence match
            case ConnectionPersistence.Session => "Connected for this session."
            case ConnectionPersistence.Device  => "Connected and remembered on this device."
          fragment(
            div.cssClass(Css.detail)(s"Connected as $login"),
            div.cssClass(Css.detail)(persistenceCopy)
          )
        case GitHubConnectionStatus.StoredCredentialRejected =>
          div.cssClass(Css.detail)("Stored credential rejected. Replace it or remove it from this device.")
    )

  private def visibility(state: GitHubConnectionStore.State): UI =
    val name = (state.status, state.busy) match
      case (GitHubConnectionStatus.Disconnected, false)             => "disconnected-idle"
      case (GitHubConnectionStatus.Disconnected, true)              => "disconnected-busy"
      case (GitHubConnectionStatus.Connected(_, _), false)          => "connected-idle"
      case (GitHubConnectionStatus.Connected(_, _), true)           => "connected-busy"
      case (GitHubConnectionStatus.StoredCredentialRejected, false) => "rejected-idle"
      case (GitHubConnectionStatus.StoredCredentialRejected, true)  => "rejected-busy"
    rawHtml(s"""<span hidden data-github-state="$name"></span>""")

  private def disconnected(
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async
  ): UI =
    div.cssClass(Css.section).id("github-disconnected")(
      div.id("github-connect-idle")(
        connectionForm(GitHubConnectionForm.Target.Connect, "Connect", fields, onConnect)
      ),
      div.id("github-connect-busy")(
        busyForm(connect = true)
      )
    )

  private def connected(onDisconnect: => Any < Async): UI =
    div.cssClass(Css.section).id("github-connected")(
      div.cssClass(Css.actions).id("github-disconnect-idle")(
        button
          .cssClass(Css.action)
          .cssClass(Css.secondary)
          .onClick(onDisconnect)("Disconnect")
      ),
      div.cssClass(Css.actions).id("github-disconnect-busy")(
        button.cssClass(Css.action).cssClass(Css.secondary).disabled(true)("Disconnecting...")
      )
    )

  private def rejected(
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async,
      onDisconnect: => Any < Async
  ): UI =
    div.cssClass(Css.section).id("github-rejected")(
      div.cssClass(Css.section).id("github-replace-idle")(
        connectionForm(GitHubConnectionForm.Target.Replace, "Replace connection", fields, onConnect),
        div.cssClass(Css.actions)(
          button
            .cssClass(Css.action)
            .cssClass(Css.secondary)
            .onClick(onDisconnect)("Remove stored credential")
        )
      ),
      div.cssClass(Css.section).id("github-replace-busy")(
        busyForm(connect = false),
        div.cssClass(Css.actions)(
          button.cssClass(Css.action).cssClass(Css.secondary).disabled(true)("Remove stored credential")
        )
      )
    )

  private def connectionForm(
      target: GitHubConnectionForm.Target,
      actionLabel: String,
      fields: GitHubConnectionForm,
      onConnect: (TokenSubmission, Boolean) => Any < Async
  ): UI =
    form
      .cssClass(Css.form)
      .id(target.formId)
      .onSubmit(submit(target, fields, onConnect))(
        label.cssClass(Css.tokenLabel).`for`(target.tokenInputId)("GitHub personal access token"),
        passwordInput(target),
        label.cssClass(Css.remember).`for`(target.rememberInputId)(
          checkbox.id(target.rememberInputId).checked(false),
          "Remember on this device"
        ),
        div.cssClass(Css.actions)(
          button.cssClass(Css.action)(actionLabel)
        )
      )

  private def passwordInput(target: GitHubConnectionForm.Target): UI =
    target match
      case GitHubConnectionForm.Target.Connect =>
        rawHtml(
          """<input id="github-connect-token" class="github-token-input" type="password" autocomplete="off" spellcheck="false" autocapitalize="none">"""
        )
      case GitHubConnectionForm.Target.Replace =>
        rawHtml(
          """<input id="github-replace-token" class="github-token-input" type="password" autocomplete="off" spellcheck="false" autocapitalize="none">"""
        )

  private def busyForm(connect: Boolean): UI =
    if connect then
      form.cssClass(Css.form).id("github-connect-busy-form")(
        label.cssClass(Css.tokenLabel).`for`("github-connect-busy-token")("GitHub personal access token"),
        rawHtml(
          """<input id="github-connect-busy-token" class="github-token-input" type="password" autocomplete="off" spellcheck="false" autocapitalize="none" disabled>"""
        ),
        label.cssClass(Css.remember).`for`("github-connect-busy-remember")(
          checkbox.id("github-connect-busy-remember").checked(false).disabled(true),
          "Remember on this device"
        ),
        div.cssClass(Css.actions)(
          button.cssClass(Css.action).disabled(true)("Connecting..."),
          span("Checking GitHub credentials...").cssClass(Css.progress)
        )
      )
    else
      form.cssClass(Css.form).id("github-replace-busy-form")(
        label.cssClass(Css.tokenLabel).`for`("github-replace-busy-token")("GitHub personal access token"),
        rawHtml(
          """<input id="github-replace-busy-token" class="github-token-input" type="password" autocomplete="off" spellcheck="false" autocapitalize="none" disabled>"""
        ),
        label.cssClass(Css.remember).`for`("github-replace-busy-remember")(
          checkbox.id("github-replace-busy-remember").checked(false).disabled(true),
          "Remember on this device"
        ),
        div.cssClass(Css.actions)(
          button.cssClass(Css.action).disabled(true)("Connecting..."),
          span("Checking GitHub credentials...").cssClass(Css.progress)
        )
      )

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Css.panel,
        Style
          .display(_.flex)
          .column
          .gap(12.px)
          .padding(16.px)
          .rounded(10.px)
          .border(1.px, Tokens.cssVar("panel-edge"))
          .bg(Tokens.cssVar("panel"))
      )
      .rule(Css.status, Style.fontSize(15.px).fontWeight(_.w600))
      .rule(Css.detail, Style.fontSize(13.px).color(Tokens.cssVar("muted")))
      .rule(Css.section, Style.display(_.flex).column.gap(12.px))
      .rule(Css.form, Style.display(_.flex).column.gap(12.px))
      .rule(Css.tokenLabel, Style.fontSize(13.px).fontWeight(_.w500))
      .rule(
        Css.token,
        Style
          .width(100.pct)
          .padding(10.px, 12.px)
          .rounded(7.px)
          .border(1.px, Tokens.cssVar("panel-edge"))
          .bg(Tokens.cssVar("bg"))
          .color(Tokens.cssVar("text"))
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
      )
      .rule(Css.remember, Style.display(_.flex).row.align(_.center).gap(8.px).fontSize(13.px))
      .rule(Css.actions, Style.display(_.flex).row.align(_.center).gap(10.px))
      .rule(
        Css.action,
        Style
          .padding(8.px, 12.px)
          .rounded(7.px)
          .border(1.px, Tokens.cssVar("accent"))
          .bg(Tokens.cssVar("accent"))
          .color(Tokens.cssVar("knob"))
          .cursor(_.pointer)
      )
      .rule(
        Css.secondary,
        Style.border(1.px, Tokens.cssVar("panel-edge")).bg(Tokens.cssVar("surface")).color(Tokens.cssVar("text"))
      )
      .rule(Css.progress, Style.fontSize(12.5.px).color(Tokens.cssVar("muted2")))
      .rule(
        Css.error,
        Style
          .fontSize(12.5.px)
          .color(Tokens.cssVar("accent-text"))
          .padding(8.px, 10.px)
          .rounded(7.px)
          .border(1.px, Tokens.cssVar("accent"))
      )
