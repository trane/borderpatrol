package com.lookout.borderpatrol.auth

import java.util.concurrent.TimeUnit
import java.util.logging.Logger

import com.lookout.borderpatrol.Binder.{BindRequest, MBinder}
import com.lookout.borderpatrol.util.Combinators._
import com.lookout.borderpatrol.{CustomerIdentifier, LoginManager, ServiceIdentifier, ServiceMatcher}
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.{Cookie, Status, Request, Response}
import com.twitter.finagle.stats.StatsReceiver
import com.twitter.finagle.{SimpleFilter, Service, Filter}
import com.twitter.logging.Level
import com.twitter.util.{Duration, Future}
import scala.util.{Failure, Success}

/**
 * PODs
 */
case class ServiceRequest(req: Request, customerId: CustomerIdentifier, serviceId: ServiceIdentifier)
case class SessionIdRequest(req: Request, customerId: CustomerIdentifier, serviceId: ServiceIdentifier,
                            sessionId: SignedId)
object SessionIdRequest {
  def apply(sr: ServiceRequest, sid: SignedId): SessionIdRequest =
    SessionIdRequest(sr.req, sr.customerId, sr.serviceId, sid)
}
case class AccessIdRequest[A](req: Request, customerId: CustomerIdentifier, serviceId: ServiceIdentifier,
                              sessionId: SignedId, id: Id[A])
object AccessIdRequest {
  def apply[A](sr: SessionIdRequest, id: Id[A]): AccessIdRequest[A] =
    AccessIdRequest(sr.req, sr.customerId, sr.serviceId, sr.sessionId, id)
}

/**
 * Determines the service that the request is trying to contact
 * If the service doesn't exist, it returns a 404 Not Found response
 *
 * @param matchers
 */
case class ServiceFilter(matchers: ServiceMatcher)
    extends Filter[Request, Response, ServiceRequest, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)

  def apply(req: Request, service: Service[ServiceRequest, Response]): Future[Response] = {
    matchers.get(req) match {
      case Some((cid, sid)) => {
        log.log(Level.DEBUG, s"Processing: Request(${req.method} " +
          s"${req.host.fold("null-hostname")(h => s"${h}${req.path}")}) " +
          s"with CustomerIdentifier: ${cid.subdomain}, ServiceIdentifier: ${sid.name}")
         service(ServiceRequest(req, cid, sid))
      }
      case None => tap(Response(Status.NotFound))(r => {
        log.log(Level.DEBUG, "Failed to find CustomerIdentifier and ServiceIdentifier for " +
          s"Request(${req.method}, ${req.host.fold("null-hostname")(h => s"${h}${req.path}")})")
        r.contentString = s"${req.path}: Unknown Path/Service(${Status.NotFound.code})"
        r.contentType = "text/plain"
      }).toFuture
    }
  }
}

/**
 * Ensures we have a SignedId present in this request, sending a Redirect to the service login page if it doesn't
 */
case class SessionIdFilter(store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[ServiceRequest, Response, SessionIdRequest, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)

  /**
   * Passes the SignedId to the next in the filter chain. If any failures decoding the SignedId occur
   * (expired, not there, etc), we will terminate early and send a redirect
   * @param req
   * @param service
   */
  def apply(req: ServiceRequest, service: Service[SessionIdRequest, Response]): Future[Response] =
    SignedId.fromRequest(req.req, SignedId.sessionIdCookieName) match {
      case Success(sid) => service(SessionIdRequest(req, sid))
      case Failure(e) =>
        for {
          session <- Session(req.req)
          _ <- store.update(session)
        } yield tap(Response(Status.Found)) { res =>
          res.location = req.customerId.loginManager.protoManager.redirectLocation(req.req.host)
          res.addCookie(session.id.asCookie())
          log.log(Level.DEBUG, s"${req.req}, allocating a new session: " +
            s"${session.id.toLogIdString}, redirecting to location: ${res.location}")
        }
    }
}

/**
 * This is a border service that glues the main chain with identityProvider or accessIssuer chains
 * E.g.
 * - If SignedId is authenticated
 *   - if path is NOT a service path, then redirect it to service identifier path
 *   - if path is a service path, then send feed it into accessIssuer chain
 * - If SignedId is NOT authenticated
 *   - if path is NOT a LoginManager path, then redirect it to LoginManager path
 *   - if path is a LoginManager path, then feed it into identityProvider chain
 *
 * @param accessIssuerMap
 * @param identityProviderMap
 */
case class BorderService(identityProviderMap: Map[String, Service[SessionIdRequest, Response]],
                         accessIssuerMap: Map[String, Service[SessionIdRequest, Response]])
    extends Service[SessionIdRequest, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)

  def servicePath(req: SessionIdRequest): Boolean =
    req.serviceId.isServicePath(Path(req.req.path))

  def loginManagerPath(req: SessionIdRequest): Boolean =
    req.customerId.isLoginManagerPath(Path(req.req.path))

  def sendToIdentityProvider(req: SessionIdRequest): Future[Response] = {
    log.log(Level.DEBUG, s"Send: ${req.req} for Session: ${req.sessionId.toLogIdString} " +
      s"to identity provider chain for service: ${req.serviceId.name}")
    identityProviderMap.get(req.customerId.loginManager.identityManager.name) match {
      case Some(ip) => ip(req)
      case None => throw IdentityProviderError(Status.NotFound, "Failed to find IdentityProvider Service Chain for " +
        req.customerId.loginManager.identityManager.name)
    }
  }

  def sendToAccessIssuer(req: SessionIdRequest): Future[Response] = {
    log.log(Level.DEBUG, s"Send: ${req.req} for Session: ${req.sessionId.toLogIdString} " +
      s"to access issuer chain for service: ${req.serviceId.name}")
    accessIssuerMap.get(req.customerId.loginManager.accessManager.name) match {
      case Some(ip) => ip(req)
      case None => throw AccessIssuerError(Status.NotFound, "Failed to find AccessIssuer Service Chain for " +
        req.customerId.loginManager.accessManager.name)
    }
  }

  def redirectTo(location: String): Response =
    tap(Response(Status.Found))(res => res.location = location)

  def redirectToService(req: SessionIdRequest): Future[Response] = {
    log.log(Level.DEBUG, s"Redirecting the ${req.req} for Authenticated Session: ${req.sessionId} " +
      s"to upstream service, location: ${req.serviceId.path}")
    redirectTo(req.serviceId.path.toString).toFuture
  }

  def redirectToLogin(req: SessionIdRequest): Future[Response] = {
    val path = req.customerId.loginManager.protoManager.redirectLocation(req.req.host)
    log.log(Level.DEBUG, s"Redirecting the ${req.req} for Untagged Session: ${req.sessionId.toLogIdString} " +
      s"to login service, location: ${path}")
    redirectTo(path).toFuture
  }

  def apply(req: SessionIdRequest): Future[Response] =
    req.sessionId.tag match {
      case AuthenticatedTag if !servicePath(req) => redirectToService(req)
      case AuthenticatedTag if servicePath(req) => sendToAccessIssuer(req)
      case Untagged if !loginManagerPath(req) => redirectToLogin(req)
      case Untagged if loginManagerPath(req) => sendToIdentityProvider(req)
    }
}

/**
 * Logout Service
 * - Deletes the session
 * - sets the empty cookie in response
 * - redirects to default service path
 */
case class LogoutService(store: SessionStore)(implicit secretStore: SecretStoreApi)
  extends Service[ServiceRequest, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)

  def apply(req: ServiceRequest): Future[Response] = {
    SignedId.fromRequest(req.req, SignedId.sessionIdCookieName).foreach(sid => {
      log.log(Level.DEBUG, s"Logging out Session: ${sid.toLogIdString}")
      store.delete(sid)
    })
    tap(Response(Status.Found)) { res =>
      res.location = req.customerId.defaultServiceId.path.toString
      // Expire all BP cookies present in the Request
      req.req.cookies.foreach[Unit] {
        case (name: String, cookie: Cookie) if name.startsWith("border_") =>
          res.addCookie(SignedId.toExpiredCookie(name))
        case _ =>
      }
      log.log(Level.DEBUG, s"W/ Session: Redirecting to default service path: ${res.location}")
    }.toFuture
  }
}

/**
 * Determines the identity of the requester, if no identity it responds with a redirect to the login page for that
 * service
 */
case class IdentityFilter[A : SessionDataEncoder](store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends Filter[SessionIdRequest, Response, AccessIdRequest[A], Response] {
  private[this] val log = Logger.getLogger(getClass.getName)

  def identity(sessionId: SignedId): Future[Identity[A]] =
    (for {
      sessionMaybe <- store.get[A](sessionId)
    } yield sessionMaybe.fold[Identity[A]](EmptyIdentity)(s => Id(s.data))) handle {
      case e => {
        log.warning(s"Failed to retrieve Identity for Session: ${sessionId}, from sessionStore with: ${e.getMessage}")
        EmptyIdentity
      }
    }

  def apply(req: SessionIdRequest, service: Service[AccessIdRequest[A], Response]): Future[Response] =
    identity(req.sessionId).flatMap(i => i match {
      case id: Id[A] => service(AccessIdRequest(req, id))
      case EmptyIdentity => for {
        s <- Session(req.req)
        _ <- store.update(s)
      } yield tap(Response(Status.Found)) { res =>
          res.location = req.customerId.loginManager.protoManager.redirectLocation(req.req.host)
          res.addCookie(s.id.asCookie()) // add SignedId value as a Cookie
          log.info(s"Failed to find Session: ${req.sessionId.toLogIdString} for Request: ${req.req}, " +
            s"allocating a new session: ${s.id.toLogIdString}, redirecting to location: ${res.location}")
        }
    })
}

/**
 * Decodes the methods Get and Post differently
 * - Get is directed to login form
 * - Post processes the login credentials
 *
 * @param binder It binds to upstream login provider using the information passed in LoginManager
 */
case class LoginManagerFilter(binder: MBinder[LoginManager])(implicit statsReceiver: StatsReceiver)
    extends Filter[SessionIdRequest, Response, SessionIdRequest, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)
  private[this] val requestSends = statsReceiver.counter("login.manager.request.sends")

  def apply(req: SessionIdRequest,
            service: Service[SessionIdRequest, Response]): Future[Response] =
    Path(req.req.path) match {
      case req.customerId.loginManager.protoManager.loginConfirm => service(req)
      case _ => {
        requestSends.incr
        log.log(Level.DEBUG, s"Send: ${req.req} for Session: ${req.sessionId.toLogIdString} " +
          s"to the Login Manager: ${req.customerId.loginManager.name}")
        binder(BindRequest(req.customerId.loginManager, req.req))
      }
    }
}

/**
 * This filter acquires the access and then forwards the request to upstream service
 *
 * @param binder It binds to the upstream service endpoint using the info passed in ServiceIdentifier
 */
case class AccessFilter[A, B](binder: MBinder[ServiceIdentifier])(implicit statsReceiver: StatsReceiver)
    extends Filter[AccessIdRequest[A], Response, AccessRequest[A], AccessResponse[B]] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)
  private[this] val requestSends = statsReceiver.counter("upstream.service.request.sends")

  def apply(req: AccessIdRequest[A],
            accessService: Service[AccessRequest[A], AccessResponse[B]]): Future[Response] =
    accessService(AccessRequest(req.id, req.customerId, req.serviceId, req.sessionId)).flatMap(
      accessResp => binder(BindRequest(req.serviceId,
        tap(req.req) { r => {
          requestSends.incr
          log.log(Level.DEBUG, s"Send: ${req.req} for Session: ${req.sessionId.toLogIdString} " +
            s"to the upstream service: ${req.serviceId.name}")
          r.headerMap.add("Auth-Token", accessResp.access.access.toString)
        }})
      )
    )
}

/**
 * This filter rewrites Request Path as per the ServiceIdentifier configuration
 */
case class RewriteFilter() extends SimpleFilter[SessionIdRequest, Response] {
  def apply(req: SessionIdRequest,
            service: Service[SessionIdRequest, Response]): Future[Response] = {
    service(SessionIdRequest(tap(req.req) { r =>
      // Rewrite the URI (i.e. path)
      r.uri = req.serviceId.rewritePath.fold(r.uri)(p =>
        r.uri.replaceFirst(req.serviceId.path.toString, p.toString))
    }, req.customerId, req.serviceId, req.sessionId))
  }
}

/**
 * Top level filter that maps exceptions into appropriate status codes
 */
case class ExceptionFilter() extends SimpleFilter[Request, Response] {
  private[this] val log = Logger.getLogger(getClass.getSimpleName)

  /**
   * Tells the service how to handle certain types of servable errors (i.e. PetstoreError)
   */
  def errorHandler: PartialFunction[Throwable, Response] = {
    case error: SessionError => {
      log.warning(error.getMessage)
      tap(Response(Status.InternalServerError))(
        r => { r.contentString = error.message; r.contentType = "text/plain" }
      )
    }
    case error: AccessDenied => {
      log.warning(error.getMessage)
      tap(Response(error.status))(
        r => { r.contentString = "AccessDenied: " + error.msg; r.contentType = "text/plain" }
      )
    }
    case error: AccessIssuerError => {
      log.warning(error.getMessage)
      tap(Response(error.status))(
        r => { r.contentString = error.msg; r.contentType = "text/plain" }
      )
    }
    case error: IdentityProviderError => {
      log.warning(error.getMessage)
      tap(Response(error.status))(
        r => { r.contentString = error.msg; r.contentType = "text/plain" }
      )
    }
    case error: Exception => {
      log.warning(error.getMessage)
      tap(Response(Status.InternalServerError))(
        r => { r.contentString = error.getMessage; r.contentType = "text/plain" }
      )
    }
  }

  def apply(req: Request, service: Service[Request, Response]): Future[Response] =
    service(req) handle errorHandler
}
