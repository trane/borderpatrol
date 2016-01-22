package com.lookout.borderpatrol.auth.keymaster

import java.util.logging.Logger

import com.lookout.borderpatrol.auth.OAuth2.OAuth2CodeVerify
import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol._
import com.lookout.borderpatrol.Binder._
import com.lookout.borderpatrol.auth._
import com.twitter.finagle.stats.StatsReceiver
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http._
import com.twitter.logging.Level
import com.twitter.util.Future


object Keymaster {
  import Tokens._

  case class KeymasterIdentifyReq(req: SessionIdRequest, credential: Credential) extends IdentifyRequest[Credential]
  case class KeymasterIdentifyRes(tokens: Tokens) extends IdentifyResponse[Tokens] {
    val identity = Identity(tokens)
  }
  case class KeymasterAccessReq(identity: Id[Tokens], customerId: CustomerIdentifier,
                                serviceId: ServiceIdentifier, sessionId: SignedId) extends AccessRequest[Tokens]
  case class KeymasterAccessRes(access: Access[ServiceToken]) extends AccessResponse[ServiceToken]

  /**
   * The identity provider for Keymaster, will connect to the remote keymaster server to authenticate and get an
   * identity (master token)
   * @param binder Binder that binds to Keymaster identity service passed in the IdentityManager
   */
  case class KeymasterIdentityProvider(binder: MBinder[Manager])
                                      (implicit statsReceiver: StatsReceiver)
      extends IdentityProvider[Credential, Tokens] {
    private[this] val log = Logger.getLogger(getClass.getSimpleName)
    private[this] val requestSends = statsReceiver.counter("keymaster.identity.provider.request.sends")
    private[this] val responseParsingFailed =
      statsReceiver.counter("keymaster.identity.provider.response.parsing.failed")
    private[this] val responseSuccess =
      statsReceiver.counter("keymaster.identity.provider.response.success")
    private[this] val responseFailed =
      statsReceiver.counter("keymaster.identity.provider.response.failed")

    /**
     * Sends credentials, if authenticated successfully will return a MasterToken otherwise a Future.exception
     */
    def apply(req: IdentifyRequest[Credential]): Future[IdentifyResponse[Tokens]] = {
      requestSends.incr

      //  Authenticate user by the Keymaster
      binder(BindRequest(req.credential.customerId.loginManager.identityManager, req.credential.toRequest))
        .flatMap(res => res.status match {
        //  Parse for Tokens if Status.Ok
        case Status.Ok =>
          Tokens.derive[Tokens](res.contentString).fold[Future[IdentifyResponse[Tokens]]](
            err => {
              responseParsingFailed.incr
              Future.exception(IdentityProviderError(Status.InternalServerError,
                "Failed to parse the Keymaster Identity Response"))
            },
            t => {
              responseSuccess.incr
              Future.value(KeymasterIdentifyRes(t))
            }
          )
        //  Preserve Response Status code by throwing AccessDenied exceptions
        case _ => {
          log.log(Level.DEBUG, s"IdentityProvider denied user: ${req.credential.uniqueId} " +
            s"with status: ${res.status} for user")
          responseFailed.incr
          Future.exception(IdentityProviderError(res.status,
            s"Invalid credentials for user ${req.credential.uniqueId}"))
        }
      })
    }
  }

  /**
   * Handles Keymaster transforms for internal and OAuth2
   */
  case class KeymasterTransformFilter(oAuth2CodeVerify: OAuth2CodeVerify)(implicit statsReceiver: StatsReceiver)
      extends Filter[SessionIdRequest, Response, KeymasterIdentifyReq, Response] {
    private[this] val log = Logger.getLogger(getClass.getSimpleName)

    def transformInternal(req: SessionIdRequest): Future[InternalAuthCredential] =
      (for {
        u <- req.req.params.get("username")
        p <- req.req.params.get("password")
      } yield InternalAuthCredential(u, p, req.customerId, req.serviceId)) match {
        case Some(c) => Future.value(c)
        case None => Future.exception(IdentityProviderError(Status.InternalServerError,
          "transformBasic: Failed to parse the Request"))
      }

    def transformOAuth2(req: SessionIdRequest, protoManager: OAuth2CodeProtoManager): Future[OAuth2CodeCredential] = {
      for {
          accessClaimSet <- oAuth2CodeVerify.codeToClaimsSet(req, protoManager)
      } yield OAuth2CodeCredential(accessClaimSet.getStringClaim("upn"), accessClaimSet.getSubject,
        req.customerId, req.serviceId)
    }

    def apply(req: SessionIdRequest,
              service: Service[KeymasterIdentifyReq, Response]): Future[Response] = {
      for {
        transformed: Credential <- req.customerId.loginManager.protoManager match {
          case a: InternalAuthProtoManager => transformInternal(req)
          case b: OAuth2CodeProtoManager => transformOAuth2(req, b)
        }
        resp <- service(KeymasterIdentifyReq(req, transformed))
      } yield resp
    }
  }

  /**
   * Handles logins to the KeymasterIdentityProvider:
   * - saves the Tokens after a successful login
   * - sends the User to their original request location from before they logged in or the default location based on
   * their service
   * @param store
   * @param secretStoreApi
   */
  case class KeymasterPostLoginFilter(store: SessionStore)
                                     (implicit secretStoreApi: SecretStoreApi, statsReceiver: StatsReceiver)
      extends Filter[KeymasterIdentifyReq, Response, IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
    private[this] val log = Logger.getLogger(getClass.getSimpleName)
    private[this] val sessionAuthenticated = statsReceiver.counter("keymaster.session.authenticated")

    /**
     * Grab the original request from the session store, otherwise just send them to the default location of '/'
     */
    def requestFromSessionStore(sessionId: SignedId): Future[Request] =
      store.get[Request](sessionId).flatMap {
        case Some(session) => Future.value(session.data)
        case None => Future.exception(OriginalRequestNotFound(s"no request stored for ${sessionId.toLogIdString}"))
      }

    def apply(req: KeymasterIdentifyReq,
              service: Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]]): Future[Response] = {
      for {
          tokenResponse <- service(req)
          session <- Session(tokenResponse.identity.id, AuthenticatedTag)
          _ <- store.update[Tokens](session)
          originReq <- requestFromSessionStore(req.req.sessionId)
          _ <- store.delete(req.req.sessionId)
        } yield tap(Response(Status.Found))(res => {
          sessionAuthenticated.incr
          res.location = originReq.uri
          res.addCookie(session.id.asCookie())
          log.log(Level.DEBUG, s"Session: ${req.req.sessionId.toLogIdString}} is authenticated, " +
            s"allocated new Session: ${session.id.toLogIdString} and redirecting to location: ${res.location}")
        })
    }
  }

  /**
   * The access issuer will use the MasterToken to gain access to service tokens
   * @param binder It binds to the Keymaster Access Issuer using info in AccessManager
   * @param store Session store
   */
  case class KeymasterAccessIssuer(binder: MBinder[Manager], store: SessionStore)
                                  (implicit statsReceiver: StatsReceiver)
      extends AccessIssuer[Tokens, ServiceToken] {
    private[this] val log = Logger.getLogger(getClass.getSimpleName)
    private[this] val requestSends = statsReceiver.counter("keymaster.access.issuer.request.sends")
    private[this] val responseParsingFailed =
      statsReceiver.counter("keymaster.access.issuer.response.parsing.failed")
    private[this] val responseSuccess =
      statsReceiver.counter("keymaster.access.issuer.response.success")
    private[this] val responseFailed =
      statsReceiver.counter("keymaster.access.issuer.response.failed")
    private[this] val cacheHits = statsReceiver.counter("keymaster.access.issuer.cache.hits")

    def api(accessRequest: AccessRequest[Tokens]): Request =
      tap(Request(Method.Post, accessRequest.customerId.loginManager.accessManager.path.toString))(req => {
        req.contentType = "application/x-www-form-urlencoded"
        req.contentString = Request.queryString("services" -> accessRequest.serviceId.name)
          .drop(1) /* Drop '?' */
        req.headerMap.add("Auth-Token", accessRequest.identity.id.master.value)
      })

    /**
     * Fetch a valid ServiceToken, will return a ServiceToken otherwise a Future.exception
     */
    def apply(req: AccessRequest[Tokens]): Future[AccessResponse[ServiceToken]] =
      //  Check if ServiceToken is already available for Service
      req.identity.id.service(req.serviceId.name).fold[Future[ServiceToken]]({
        requestSends.incr

        //  Fetch ServiceToken from the Keymaster
        binder(BindRequest(req.customerId.loginManager.accessManager, api(req))).flatMap(res => res.status match {
          //  Parse for Tokens if Status.Ok
          case Status.Ok =>
            Tokens.derive[Tokens](res.contentString).fold[Future[ServiceToken]](
              e => {
                responseParsingFailed.incr
                Future.exception(AccessIssuerError(Status.NotAcceptable,
                  "Failed to parse the Keymaster Access Response"))
              },
              t => {
                responseSuccess.incr
                t.service(req.serviceId.name).fold[Future[ServiceToken]](
                  Future.exception(AccessDenied(Status.NotAcceptable,
                    s"No access allowed to service ${req.serviceId.name}"))
                )(st => for {
                  _ <- store.update(Session(req.sessionId, req.identity.id.add(req.serviceId.name, st)))
                } yield st)
              }
            )
          //  Preserve Response Status code by throwing AccessDenied exceptions
          case _ => {
            log.log(Level.DEBUG, s"AccessIssuer denied access to service: ${req.serviceId.name} " +
              s"with status: ${res.status}")
            responseFailed.incr
            Future.exception(AccessIssuerError(res.status,
              s"No access allowed to service ${req.serviceId.name} due to error: ${res.status}"))
          }
        })
      })(t => {
        cacheHits.incr
        Future.value(t)
      }).map(t => KeymasterAccessRes(Access(t)))
  }

  /**
   *  Keymaster Identity provider service Chain
   * @param store
   */
  def keymasterIdentityProviderChain(store: SessionStore)(
    implicit secretStoreApi: SecretStoreApi, statsReceiver: StatsReceiver): Service[SessionIdRequest, Response] =
      LoginManagerFilter(LoginManagerBinder) andThen
        KeymasterTransformFilter(new OAuth2CodeVerify) andThen
        KeymasterPostLoginFilter(store) andThen
        KeymasterIdentityProvider(ManagerBinder)


  /**
   * Keymaster Access Issuer service Chain
   * @param store
   */
  def keymasterAccessIssuerChain(store: SessionStore)(
    implicit secretStoreApi: SecretStoreApi, statsReceiver: StatsReceiver): Service[SessionIdRequest, Response] =
      RewriteFilter() andThen
        IdentityFilter[Tokens](store) andThen
        AccessFilter[Tokens, ServiceToken](ServiceIdentifierBinder) andThen
        KeymasterAccessIssuer(ManagerBinder, store)

}
