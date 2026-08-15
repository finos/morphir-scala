package morphir.appkit

/** Failures a secret store can report. Usable as an exception at a user-facing boundary. */
enum SecretError(message: String) extends Exception(message):
  case NotAvailable(detail: String) extends SecretError(detail)
  case LookupFailed(detail: String) extends SecretError(detail)
