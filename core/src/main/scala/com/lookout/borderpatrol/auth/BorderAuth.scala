package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.{ServiceIdentifier, ServiceMatcher}

import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.httpx.{Status,  Request, Response}
import com.twitter.finagle.{Service, Filter}
import com.twitter.util.Future

import scala.util.{Failure, Success}

/**
 * Given an authenticated route/endpoint, this type class will allow us to handle two use cases
 *  - Identified entity is asking for access to service at authenticated route/endpoint
 *  - Entity has not identified itself, must be prompted to identify
 */
trait BorderAuth {
  def identify[A, B](req: Request, idp: IdentityProvider[A, B]): Future[Identity[B]]
  def issueAccess[A](a: A): Future[Access[A]]
}

case class BorderRequest[A](access: Access[A], request: Request)
case class ServiceRequest(req: Request, id: ServiceIdentifier)
//case class SessionIdRequest(req: Request, sid: SessionId)

/**
 * Given an incoming request to an authenticated endpoint, this filter has two duties:
 *  - If there is no identity:
 *    - we must send the browser a redirect to a page where we can get credentials
 *    - save the requested url with their session so that once they have authenticated, we can send them there
 *  - If there is an identity, i.e. they have a sessionid with some stored auth `Identity[A]`:
 *    - get `Access[A]` for the service they are requesting, either already cached in their session or by asking
 *    the `AccessIssuer` for that access
 *    - issue the request to the downstream service with that access injected
 */
class BorderFilter[A](store: SessionStore)
    extends Filter[AccessRequest[A], Response, BorderRequest[A], Response] {

  def apply(req: AccessRequest[A], service: Service[BorderRequest[A], Response]): Future[Response] =
    sys.error("not implemented")
}

/**
 * Determines the service that the request is trying to contact
 * If the service doesn't exist, it returns a 404 Not Found response
 *
 * @param matchers
 *
 */
class ServiceFilter(matchers: ServiceMatcher)
    extends Filter[Request, Response, ServiceRequest, Response] {

  def apply(req: Request, service: Service[ServiceRequest, Response]): Future[Response] =
    matchers.get(req) match {
      case Some(id) => service(ServiceRequest(req, id))
      case None => Future.value(Response(Status.NotFound))
    }
}

/**
 * Determines the identity of the requester, if no identity it responds with a redirect to the login page for that
 * service
 *
 * Note: this filter does not handle the POST to login url with identity information
 */
class IdentityFilter[A : SessionDataEncoder](store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[ServiceRequest, Response, AccessRequest[A], Response] {

  /**
   * Get any existing identity for this request, or EmptyIdentity.
   * This happens two ways:
   *  1) If the request has a SessionId, we check the SessionStore for the identity yielding an EmptyIdentity if it
   *  doesn't exist
   *  2) If there is no SessionId we skip the lookup from the SessionStore
   */
  def identity(req: ServiceRequest): Future[Identity[A]] =
    (for {
      sessionId <- SessionId.fromRequest(req.req).toFuture
      sessionMaybe <- store.get[A](sessionId)
    } yield sessionMaybe.fold[Identity[A]](EmptyIdentity)(s => Id(s.data))) handle {
      case e => EmptyIdentity //***FIXME: We need a log here
    }

  def apply(req: ServiceRequest, service: Service[AccessRequest[A], Response]): Future[Response] =
    identity(req).flatMap(i => i match {
      case id: Id[A] => service(AccessRequest(id, req.id))
      case EmptyIdentity => for {
        s <- Session[Request](req.req)
        _ <- store.update(s)
      } yield tap(Response(Status.TemporaryRedirect)) { res =>
          res.location = req.id.login // set to login url
          res.addCookie(s.id.asCookie) // add SessionId value as a Cookie
        }
    })
}
