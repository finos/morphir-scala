package morphir.appkit

import morphir.MorphirException

/** Failures a secret store can report. Usable as an exception at a user-facing boundary. */
enum SecretException(message: String) extends MorphirException(message):
  case NotAvailable(detail: String)      extends SecretException(detail)
  case LookupFailed(detail: String)      extends SecretException(detail)
  case MutationFailed(operation: String) extends SecretException(s"System keyring $operation failed")
