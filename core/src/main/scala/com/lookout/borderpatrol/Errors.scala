package com.lookout.borderpatrol

class BaseError(val message: String, cause: Throwable) extends Exception(message, cause) {
  // scalastyle:off null
  def this(message: String) = this(message, null)
}

case class CommunicationError(error: String)
  extends BaseError(s"An error occurred while talking to: $error")
