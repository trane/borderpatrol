package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.ServiceIdentifier

class AuthError(val message: String, cause: Throwable) extends Exception(message, cause) {
  // scalastyle:off null
  def this(message: String) = this(message, null)
}

/**
 * A response to the user that informs them what to do next when they try to access a protected resource
 * Example: In the case of SAML it would be an http redirect to their IdP
 */
case class IdentityRequired(id: ServiceIdentifier, cause: Throwable = new Exception) extends AuthError("", cause)
