package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.{ServiceIdentifier, ServiceMatcher}
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.httpx.{Method, Status, Request, Response}
import com.twitter.finagle.{Service, Filter}
import com.twitter.util.Future
import scala.util.{Failure, Success}

/**
 * PODs
 */
case class ServiceRequest(req: Request, serviceId: ServiceIdentifier)
case class SessionIdRequest(req: ServiceRequest, sessionId: SessionId)
case class AccessIdRequest[A](req: SessionIdRequest, id: Id[A])

/**
 * Determines the service that the request is trying to contact
 * If the service doesn't exist, it returns a 404 Not Found response
 *
 * @param matchers
 */
class ServiceFilter(matchers: ServiceMatcher)
    extends Filter[Request, Response, ServiceRequest, Response] {

  def apply(req: Request, service: Service[ServiceRequest, Response]): Future[Response] =
    matchers.get(req) match {
      case Some(id) => service(ServiceRequest(req, id))
      case None => tap(Response(Status.NotFound))(r => {
        r.contentString = s"${req.path}: Unknown Path/Service(${Status.NotFound.code})"
        r.contentType = "text/plain"
      }).toFuture
    }
}

/**
 * Ensures we have a SessionId present in this request, sending a Redirect to the service login page if it doesn't
 */
case class SessionIdFilter(store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[ServiceRequest, Response, SessionIdRequest, Response] {

  /**
   * Passes the SessionId to the next in the filter chain. If any failures decoding the SessionId occur
   * (expired, not there, etc), we will terminate early and send a redirect
   * @param req
   * @param service
   */
  def apply(req: ServiceRequest, service: Service[SessionIdRequest, Response]): Future[Response] =
    SessionId.fromRequest(req.req) match {
      case Success(sid) => service(SessionIdRequest(req, sid))
      case Failure(e) =>
        for {
          session <- Session(req.req)
          _ <- store.update(session)
        } yield tap(Response(Status.Found)) { res =>
          res.location = req.serviceId.loginManager.path.toString
          res.addCookie(session.id.asCookie)
        }
    }
}

/**
 * Determine if the session is tagged (i.e. authenticated) or not and force changes to request path
 * E.g.
 * - If SessionId is authenticated and the path is NOT a service path, then redirect it to service identifier path
 * - If SessionId is NOT authenticated and path is a service path, then redirect to login page
 */
class BorderFilter extends Filter[SessionIdRequest, Response, SessionIdRequest, Response] {

  def servicePath(req: SessionIdRequest): Boolean =
    req.req.serviceId.isServicePath(Path(req.req.req.path))

  def redirectTo(path: Path): Response =
    tap(Response(Status.Found))(res => res.location = path.toString)

  def redirectToService(req: SessionIdRequest): Future[Response] =
    redirectTo(req.req.serviceId.path).toFuture

  def redirectToLogin(req: SessionIdRequest): Future[Response] =
    redirectTo(req.req.serviceId.loginManager.path).toFuture

  def apply(req: SessionIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] =
    req.sessionId.tag match {
      case AuthenticatedTag if !servicePath(req) => redirectToService(req)
      case Untagged if servicePath(req) => redirectToLogin(req)
      case _ => service(req)
    }
}

/**
 * Determines the identity of the requester, if no identity it responds with a redirect to the login page for that
 * service
 */
class IdentityFilter[A : SessionDataEncoder](store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[SessionIdRequest, Response, AccessIdRequest[A], Response] {

  def identity(sid: SessionId): Future[Identity[A]] =
    (for {
      sessionMaybe <- store.get[A](sid)
    } yield sessionMaybe.fold[Identity[A]](EmptyIdentity)(s => Id(s.data))) handle {
      case e => EmptyIdentity
    }

  def apply(req: SessionIdRequest, service: Service[AccessIdRequest[A], Response]): Future[Response] =
    identity(req.sessionId).flatMap(i => i match {
      case id: Id[A] => service(AccessIdRequest(req, id))
      case EmptyIdentity => for {
        s <- Session(req.req.req)
        _ <- store.update(s)
      } yield tap(Response(Status.Found)) { res =>
          res.location = req.req.serviceId.loginManager.path.toString // set to login url
          res.addCookie(s.id.asCookie) // add SessionId value as a Cookie
        }
    })
}

/**
 * Top level filter that maps exceptions into appropriate status codes
 */
class ExceptionFilter
    extends Filter[Request, Response, Request, Response] {

  /**
   * Tells the service how to handle certain types of servable errors (i.e. PetstoreError)
   */
  def errorHandler: PartialFunction[Throwable, Response] = {
    case error: SessionError => tap(Response(Status.InternalServerError))(
      r => { r.contentString = error.message; r.contentType = "text/plain"}
    )
    case error: AccessDenied => tap(Response(error.status))(
      r => { r.contentString = "AccessDenied: " + error.msg; r.contentType = "text/plain"}
    )
    case error: AccessIssuerError => tap(Response(error.status))(
      r => { r.contentString = error.msg; r.contentType = "text/plain"}
    )
    case error: IdentityProviderError => tap(Response(error.status))(
      r => { r.contentString = error.msg; r.contentType = "text/plain"}
    )
    case error: Exception => tap(Response(Status.InternalServerError))(
      r => { r.contentString = error.getMessage; r.contentType = "text/plain"}
    )
  }

  def apply(req: Request, service: Service[Request, Response]): Future[Response] =
    service(req) handle errorHandler
}
