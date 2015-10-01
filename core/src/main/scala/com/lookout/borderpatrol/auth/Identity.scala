package com.lookout.borderpatrol.auth

import com.twitter.finagle.Service

/**
 * The purpose of this abstraction to return some input from a user or entity that can be transformed into an
 * understandable identity to be used in the `AccessRequest`. There is no requirement that the identity from the
 * identifier service be the same as the one sent to the access issuer, however, you must supply a transformation.
 *
 * Use case:
 *
 * SAML:
 *  Issue an `IdentityRequired` if there is no valid session, redirecting to the user's IdP
 *  Have an endpoint that recieves the POST with `IdentifyResponse[SamlToken]`
 *  Hand off the `Identity[SamlToken]` to the `AccessIssuer`
 *
 * Internal Identity Provider:
 *  Issue an `IdentityRequired` if there is no valid session, redirect to the login service
 *  Intercept the POST with credentials and forward them to the `IdentityProvider`
 *  Receive a `IdentifyResponse[?]` directly from the `IdentityProvider`
 *  Hand off the `Identity` to the `AccessIssuer`
 */

/**
 * This encapsulates the notion of an identifier that the AccessIssuer can understand.
 * In the case of OAuth2 we would wrap a the Access Token grant, or for SAML we would wrap the SAML token, then we
 * hand this off to the [[com.lookout.borderpatrol.auth.AccessIssuer AccessIssuer]]
 */
sealed trait Identity[+A]
case object EmptyIdentity extends Identity[Nothing]
case class Id[+A](id: A) extends Identity[A]

object Identity {
  def apply[A](a: A): Id[A] =
    Id(a)
}


/**
 * A request to gain an `Identity`, e.g. email/password credentials
 *
 * Note: this wouldn't be used for most cases of something providing external authentication, like in the case of
 * SAML, since the user would have been redirected to an external IdP for logging in.
 */
trait IdentifyRequest[A] {
  val credential: A
}

/**
 * A response from the identity provider with some identity
 *
 * Example: SAML POST response to a successful login to a third party IdP
 */
trait IdentifyResponse[A] {
  val identity: Id[A]
}

/**
 * Abstraction for those that are directing requests directly to the Identity Provider
 */
trait IdentityProvider[A, B] extends Service[IdentifyRequest[A], IdentifyResponse[B]]
