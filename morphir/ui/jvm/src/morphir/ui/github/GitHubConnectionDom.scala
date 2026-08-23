package morphir.ui.github

import kyo.*

private[github] object GitHubConnectionDom:
  val form: GitHubConnectionForm = new GitHubConnectionForm:
    def capture(target: GitHubConnectionForm.Target): GitHubConnectionSubmission < Sync =
      Sync.defer(throw UnsupportedOperationException(s"No DOM form is available for ${target.formId} on the JVM"))
