package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.{CustomerIdentifier, ServiceIdentifier}
import com.lookout.borderpatrol.sessionx.SignedId
import com.twitter.finagle.Service

/**
 * The purpose of this module is to transpose an incoming identified request into a request that the `AccessIssuer`
 * understands, send that request to the `AccessIssuer` and receive a reply with access granted or not.
 *
 * We want to be able to transform the (identity, service, request) tuple into an understandable form for the
 * `AccessIssuer`
 *
 * {{{
 *   case class Credential(user: String, password: String)
 *
 *   def httpBasicAccess(cred: Credential, req: http.Request): AccessRequest[String] =
 *      new AccessRequest[String] {
 *        val identity = s"${cred.user}:${cred.password}"
 *        val serviceId = "example"
 *        val request = req
 *
 *        def basicRequest: Request = {
 *          request.headers += ("Basic" -> Base64Encoder(credential))
 *          request
 *        }
 *      }
 *
 *   case class ApiToken(token: String)
 *   case class TokenAccessResponse(access: Option[ApiToken], reply: http.Response) extends AccessResponse[ApiToken]
 *
 *   case class ApiTokenIssuer(remote: Service[http.Request, http.Response])
 *       extends AccessIssuer[String, ApiToken] {
 *
 *     def apply(req: AccessRequest[String]): Future[AccessResponse[ApiToken]] =
 *       remote(req.baseRequest).map(res => TokenAccessResponse(res.body.as[ApiToken], res))
 *   }
 * }}}
 */

/**
 * Abstraction for some access data, e.g. service token, grant, role, scope
 */
trait Access[A] {
  val access: A
}

object Access {
  def apply[A](a: A): Access[A] =
    new Access[A] {val access = a}
}

/**
 * The identification information needed by the [[com.lookout.borderpatrol.auth.AccessIssuer AccessIssuer]]
 * to issue access data for your request
 *
 * This can be thought of as a function (A, ServiceIdentifier) => Req
 */
trait AccessRequest[A] {
  val identity: Id[A]
  val customerId: CustomerIdentifier
  val serviceId: ServiceIdentifier
  val sessionId: SignedId
}

object AccessRequest {
  def apply[A](id: Id[A], custId: CustomerIdentifier, servId: ServiceIdentifier, sessId: SignedId): AccessRequest[A] =
    new AccessRequest[A] {
      val identity = id
      val customerId = custId
      val serviceId = servId
      val sessionId = sessId
    }
}

/**
 * This response contains the access data needed by an authenticated endpoint, e.g. grants, tokens, api keys
 */
trait AccessResponse[A] {
  val access: Access[A]
}

/**
 * Describes a service that acts as an Access issuing endpoint, this would be something like an OAuth2 token
 * service, or an LDAP server, or a database that holds access tokens for user credentials
 */
trait AccessIssuer[A, B] extends Service[AccessRequest[A], AccessResponse[B]]
