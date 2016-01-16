package com.lookout.borderpatrol.sessionx

class SessionError(val message: String, cause: Throwable) extends Exception(message, cause) {
  // scalastyle:off null
  def this(message: String) = this(message, null)
}

case class SessionIdError(error: String)
    extends SessionError(s"An error occurred reading SessionId: $error")

case class SessionDataError(error: Throwable)
    extends SessionError(s"An error occurred reading Session data: ${error.getMessage}")

case class SessionStoreError(msg: String)
    extends SessionError(s"An error occurred interacting with the session store: $msg")

case class SecretDecodeError(msg: String)
    extends SessionError(s"An error decoding a Secret occurred: $msg")

case class SecretsDecodeError(msg: String)
  extends SessionError(s"An error decoding a Secrets occurred: $msg")

case class OriginalRequestNotFound(msg: String)
  extends SessionError(s"An error occurred interacting with the session store: $msg")

case class ConsulError(msg: String)
  extends SessionError(s"An error occurred getting a value from Consul: $msg")

case class SessionCreateUnavailable(msg: String)
  extends SessionError(s"Session create failed: $msg")
