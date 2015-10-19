package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.ServiceIdentifier
import com.lookout.borderpatrol.auth._
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.httpx._
import com.twitter.util.Future


object Keymaster {
  case class Credential(email: String, password: String, serviceId: ServiceIdentifier)
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
   * @param client Keymaster service
   * @param path Keymaster service endpoint path
   */
  case class KeymasterIdentityProvider(client: Service[Request, Response], path: Path)
    extends IdentityProvider[Credential, Tokens] {

    def api(cred: Credential): Request =
      Request(Method.Post,
        Request.queryString(path.toString, ("e", cred.email), ("p", cred.password), ("s", cred.serviceId.name)))

    /**
     * Sends credentials, if authenticated successfully will return a MasterToken otherwise a Future.exception
     */
    def apply(req: IdentifyRequest[Credential]): Future[IdentifyResponse[Tokens]] =
      //  Authenticate user by the Keymaster
      client(api(req.credential)).flatMap(res => res.status match {
        //  Parse for Tokens if Status.Ok
        case Status.Ok =>
          Tokens.derive[Tokens](res.contentString).fold[Future[IdentifyResponse[Tokens]]](
            err => Future.exception(IdentityProviderError(Status.InternalServerError,
              "Failed to parse the Keymaster Identity Response")),
            t => Future.value(KeymasterIdentifyRes(t))
          )
        //  Preserve Response Status code by throwing AccessDenied exceptions
        case _ => Future.exception(IdentityProviderError(res.status,
            s"Invalid credentials for user ${req.credential.email}"))
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
  case class KeymasterLoginFilter(store: SessionStore)(implicit secretStoreApi: SecretStoreApi)
    extends Filter[SessionIdRequest, Response, IdentifyRequest[Credential], IdentifyResponse[Tokens]] {

    def createIdentifyReq(req: SessionIdRequest): Option[IdentifyRequest[Credential]] =
      for {
        u <- req.req.req.params.get("username")
        p <- req.req.req.params.get("password")
      } yield KeymasterIdentifyReq(Credential(u, p, req.req.serviceId))

    /**
     * Grab the original request from the session store, otherwise just send them to the default location of '/'
     */
    def getRequestFromSessionStore(id: SessionId): Future[Request] =
      store.get[Request](id).flatMap(_ match {
        case Some(session) => Future.value(session.data)
        case None => Future.exception(OriginalRequestNotFound(s"no request stored for $id"))
      })

    def apply(req: SessionIdRequest,
              service: Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]]): Future[Response] =
      createIdentifyReq(req).fold(Future.value(Response(Status.BadRequest)))(credReq =>
        for {
          originReq <- getRequestFromSessionStore(req.sid)
          tokenResponse <- service(credReq)
          session <- Session(tokenResponse.identity.id)
          _ <- store.update[Tokens](session)
          _ <- store.delete(req.sid)
        } yield tap(Response(Status.TemporaryRedirect))(res => {
          res.location = originReq.uri
          res.addCookie(session.id.asCookie)
        })
      )
  }

  /**
   * The access issuer will use the MasterToken to gain access to service tokens
   * @param client Keymaster service
   * @param path Keymaster service endpoint path
   * @param store Session store
   */
  case class KeymasterAccessIssuer(client: Service[Request, Response], path: Path, store: SessionStore)
    extends AccessIssuer[Tokens, ServiceToken] {
    def api(req: AccessRequest[Tokens]): Request =
      tap(Request(Method.Post, Request.queryString(path.toString(),
        ("services" -> req.serviceId.name))))(r => r.headerMap.add("Auth-Token", req.identity.id.master.value))

    /**
     * Fetch a valid ServiceToken, will return a ServiceToken otherwise a Future.exception
     */
    def apply(req: AccessRequest[Tokens]): Future[AccessResponse[ServiceToken]] =
      //  Check if ServiceToken is already available for Service
      req.identity.id.service(req.serviceId.name).fold[Future[ServiceToken]](
        //  Fetch ServiceToken from the Keymaster
        client(api(req)).flatMap(res => res.status match {
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
            s"No access allowed to service ${req.serviceId.name}"))
        })
      )(t => Future.value(t)).map(t => KeymasterAccessRes(Access(t)))
  }

  /**
   * This filter acquires the access and then forwards the request to upstream service
   *
   * @param upstreamClient
   */
  case class KeymasterAccessFilter(upstreamClient: Service[Request, Response])
    extends Filter[AccessIdRequest[Tokens], Response, AccessRequest[Tokens], AccessResponse[ServiceToken]] {
    def apply(req: AccessIdRequest[Tokens],
              accessService: Service[AccessRequest[Tokens], AccessResponse[ServiceToken]]): Future[Response] =
      accessService(AccessRequest(req.id, req.req.req.serviceId, req.req.sid)).flatMap(accessResp =>
        upstreamClient(tap(req.req.req.req) { r => r.headerMap.add("Auth-Token", accessResp.access.access.value)})
      )
  }
}
