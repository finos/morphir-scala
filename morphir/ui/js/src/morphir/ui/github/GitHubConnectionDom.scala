package morphir.ui.github

import kyo.*
import org.scalajs.dom.{document, HTMLElement, HTMLInputElement}

private[github] object GitHubConnectionDom:
  val form: GitHubConnectionForm = new GitHubConnectionForm:
    def capture(target: GitHubConnectionForm.Target): GitHubConnectionSubmission < Sync = Sync.defer {
      val form     = document.getElementById(target.formId).asInstanceOf[HTMLElement]
      val token    = document.getElementById(target.tokenInputId).asInstanceOf[HTMLInputElement]
      val remember = document.getElementById(target.rememberInputId).asInstanceOf[HTMLInputElement]

      if form == null || token == null || remember == null || !form.contains(token) || !form.contains(remember) then
        throw IllegalStateException(s"GitHub connection form is incomplete: ${target.formId}")

      new GitHubConnectionSubmission:
        def tokenValue: String < Sync = Sync.defer(token.value)

        def rememberChecked: Boolean < Sync = Sync.defer(remember.checked)

        def clearToken: Unit < Sync = Sync.defer {
          token.value = ""
        }
    }
