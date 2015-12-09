package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.ServiceIdentifier
import com.lookout.borderpatrol.{InternalAuthProtoManager, Manager, OAuth2CodeProtoManager}
import com.lookout.borderpatrol.Binder._
import com.lookout.borderpatrol.auth._
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.httpx._
import com.twitter.util.Future


object Keymaster {
  import Tokens._
  import KeymasterTransform._

  case class KeymasterIdentifyReq(credential: Credential) extends IdentifyRequest[Credential]
  case class KeymasterIdentifyRes(tokens: Tokens) extends IdentifyResponse[Tokens] {
    val identity = Identity(tokens)
  }
  case class KeymasterAccessReq(identity: Id[Tokens],
                                serviceId: ServiceIdentifier, sessionId: SessionId) extends AccessRequest[Tokens]
  case class KeymasterAccessRes(access: Access[ServiceToken]) extends AccessResponse[ServiceToken]

  /**
   * The identity provider for Keymaster, will connect to the remote keymaster server to authenticate and get an
   * identity (master token)
   * @param binder Binder that binds to Keymaster identity service passed in the IdentityManager
   */
  case class KeymasterIdentityProvider(binder: MBinder[Manager])
      extends IdentityProvider[Credential, Tokens] {

    /**
     * Sends credentials, if authenticated successfully will return a MasterToken otherwise a Future.exception
     */
    def apply(req: IdentifyRequest[Credential]): Future[IdentifyResponse[Tokens]] =
      //  Authenticate user by the Keymaster
      binder(BindRequest(req.credential.serviceId.loginManager.identityManager, req.credential.toRequest))
        .flatMap(res => res.status match {
        //  Parse for Tokens if Status.Ok
        case Status.Ok =>
          Tokens.derive[Tokens](res.contentString).fold[Future[IdentifyResponse[Tokens]]](
            err => Future.exception(IdentityProviderError(Status.InternalServerError,
              "Failed to parse the Keymaster Identity Response")),
            t => Future.value(KeymasterIdentifyRes(t))
          )
        //  Preserve Response Status code by throwing AccessDenied exceptions
        case _ => Future.exception(IdentityProviderError(res.status,
          s"Invalid credentials for user ${req.credential.uniqueId}"))
      })
  }

  /**
   * Handles logins to the KeymasterIdentityProvider:
   * - saves the Tokens after a successful login
   * - sends the User to their original request location from before they logged in or the default location based on
   * their service
   * @param store
   * @param secretStoreApi
   */
  case class KeymasterPostLoginFilter(store: SessionStore)(implicit secretStoreApi: SecretStoreApi)
      extends Filter[SessionIdRequest, Response, IdentifyRequest[Credential], IdentifyResponse[Tokens]] {

    /**
     * Grab the original request from the session store, otherwise just send them to the default location of '/'
     */
    def requestFromSessionStore(id: SessionId): Future[Request] =
      store.get[Request](id).flatMap {
        case Some(session) => Future.value(session.data)
        case None => Future.exception(OriginalRequestNotFound(s"no request stored for $id"))
      }

    def apply(req: SessionIdRequest,
              service: Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]]): Future[Response] = {
      for {
          transformed: Credential <- req.req.serviceId.loginManager.protoManager match {
            case a: InternalAuthProtoManager => a.transform[SessionIdRequest, InternalAuthCredential](req)
            case b: OAuth2CodeProtoManager => b.transform[SessionIdRequest, OAuth2CodeCredential](req)
          }
          tokenResponse <- service(KeymasterIdentifyReq(transformed))
          session <- Session(tokenResponse.identity.id, AuthenticatedTag)
          _ <- store.update[Tokens](session)
          originReq <- requestFromSessionStore(req.sessionId)
          _ <- store.delete(req.sessionId)
        } yield tap(Response(Status.Found))(res => {
          res.location = originReq.uri
          res.addCookie(session.id.asCookie)
        })
    }
  }

  /**
   * The access issuer will use the MasterToken to gain access to service tokens
   * @param binder It binds to the Keymaster Access Issuer using info in AccessManager
   * @param store Session store
   */
  case class KeymasterAccessIssuer(binder: MBinder[Manager], store: SessionStore)
      extends AccessIssuer[Tokens, ServiceToken] {
    def api(accessRequest: AccessRequest[Tokens]): Request =
      tap(Request(Method.Post, accessRequest.serviceId.loginManager.accessManager.path.toString))(req => {
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
      req.identity.id.service(req.serviceId.name).fold[Future[ServiceToken]](
        //  Fetch ServiceToken from the Keymaster
        binder(BindRequest(req.serviceId.loginManager.accessManager, api(req))).flatMap(res => res.status match {
          //  Parse for Tokens if Status.Ok
          case Status.Ok =>
            Tokens.derive[Tokens](res.contentString).fold[Future[ServiceToken]](
              e => Future.exception(AccessIssuerError(Status.NotAcceptable,
                "Failed to parse the Keymaster Access Response")),
              t => t.service(req.serviceId.name).fold[Future[ServiceToken]](
                Future.exception(AccessDenied(Status.NotAcceptable,
                  s"No access allowed to service ${req.serviceId.name}"))
              )(st => for {
                _ <- store.update(Session(req.sessionId, req.identity.id.add(req.serviceId.name, st)))
              } yield st)
            )
          //  Preserve Response Status code by throwing AccessDenied exceptions
          case _ => Future.exception(AccessIssuerError(res.status,
            s"No access allowed to service ${req.serviceId.name} due to error: ${res.status}"))
        })
      )(t => Future.value(t)).map(t => KeymasterAccessRes(Access(t)))
  }

  /**
   *  Keymaster Identity provider service Chain
   * @param store
   */
  def keymasterIdentityProviderChain(store: SessionStore)(
    implicit secretStoreApi: SecretStoreApi): Service[SessionIdRequest, Response] = {
    LoginManagerFilter(LoginManagerBinder) andThen
      KeymasterPostLoginFilter(store) andThen
      KeymasterIdentityProvider(ManagerBinder)
  }

  /**
   * Keymaster Access Issuer service Chain
   * @param store
   */
  def keymasterAccessIssuerChain(store: SessionStore)(
    implicit secretStoreApi: SecretStoreApi): Service[SessionIdRequest, Response] = {
    IdentityFilter[Tokens](store) andThen
      AccessFilter[Tokens, ServiceToken](ServiceIdentifierBinder) andThen
      KeymasterAccessIssuer(ManagerBinder, store)
  }
}
