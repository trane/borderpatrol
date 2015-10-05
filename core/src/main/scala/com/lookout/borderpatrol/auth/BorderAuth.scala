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
case class SessionIdRequest(req: ServiceRequest, sid: SessionId)

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
 * Ensures we have a SessionId present in this request, sending a Redirect to the service login page if it doesn't
 */
case class SessionIdFilter(store: SessionStore)(implicit secretStore: SecretStoreApi)
  extends Filter[ServiceRequest, Response, SessionIdRequest, Response] {

  def apply(req: ServiceRequest, service: Service[SessionIdRequest, Response]): Future[Response] =
    SessionId.fromRequest(req.req) match {
      //  Propagate the failures from "service" to the caller
      case Success(id) => service(SessionIdRequest(req, id))
      //  For all failures encountered while decoding SessionId from request, send redirects
      case Failure(e) =>
        for {
          session <- Session(req.req)
          _ <- store.update(session)
        } yield tap(Response(Status.TemporaryRedirect)) { res =>
          res.location = req.id.login
          res.addCookie(session.id.asCookie)
        }
    }
}

/**
 * Determines the identity of the requester, if no identity it responds with a redirect to the login page for that
 * service
 *
 * Note: this filter does not handle the POST to login url with identity information
 */
class IdentityFilter[A : SessionDataEncoder](store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[SessionIdRequest, Response, AccessRequest[A], Response] {

  def identity(sid: SessionId): Future[Identity[A]] =
    for {
      sessionMaybe <- store.get[A](sid)
    } yield sessionMaybe.fold[Identity[A]](EmptyIdentity)(s => Id(s.data))

  def apply(req: SessionIdRequest, service: Service[AccessRequest[A], Response]): Future[Response] =
    identity(req.sid).flatMap(i => i match {
      case id: Id[A] => service(AccessRequest(id, req.req.id, req.sid))
      case EmptyIdentity => for {
        s <- Session(req.req.req)
        _ <- store.update(s)
      } yield tap(Response(Status.TemporaryRedirect)) { res =>
          res.location = req.req.id.login // set to login url
          res.addCookie(s.id.asCookie) // add SessionId value as a Cookie
        }
    })
}
