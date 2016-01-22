package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.ServiceIdentifier
import com.twitter.finagle.http.Status

class AuthError(val message: String, cause: Throwable) extends Exception(message, cause) {
  // scalastyle:off null
  def this(message: String) = this(message, null)
}

/**
 * A response to the user that informs them what to do next when they try to access a protected resource
 * Example: In the case of SAML it would be an http redirect to their IdP
 */
case class IdentityRequired(id: ServiceIdentifier, cause: Throwable = new Exception) extends AuthError("", cause)

/**
 * This exception stores the response code
 */
case class AccessDenied(status: Status, msg: String) extends AuthError(msg, null)

/**
 * This exception stores the response code
 */
case class IdentityProviderError(status: Status, msg: String) extends AuthError(msg, null)

/**
 * This exception stores the response code
 */
case class AccessIssuerError(status: Status, msg: String) extends AuthError(msg, null)

/**
 * Token Parsing error
 */
case class BpTokenParsingError(msg: String)
    extends AuthError(s"Failed to parse token with: ${msg}", null)

/**
 * Certificate processing error
 */
case class BpCertificateError(msg: String)
    extends AuthError(s"Failed to process Certificate with: ${msg}}", null)
