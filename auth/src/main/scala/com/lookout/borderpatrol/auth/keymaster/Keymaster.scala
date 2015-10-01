package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.util.Combinators.tap

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.{sessionx, ServiceIdentifier}
import com.lookout.borderpatrol.auth._
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.httpx.{Status, RequestBuilder, Response, Request}
import com.twitter.util.Future

object Keymaster {
  case class Credential(email: String, password: String, serviceId: ServiceIdentifier)

  class IdentifyException extends Exception("")
  case object AccessDenied extends Exception("no access available for this service")

  case class KeymasterIdentifyReq(credential: Credential) extends IdentifyRequest[Credential]
  case class KeymasterIdentifyRes(tokens: Tokens) extends IdentifyResponse[Tokens] {
    val identity = Identity(tokens)
  }
  case class KeymasterAccessReq(identity: Id[Tokens], serviceId: ServiceIdentifier) extends AccessRequest[Tokens]
  case class KeymasterAccessRes(access: Access[ServiceToken]) extends AccessResponse[ServiceToken]

  /**
   * The identity provider for Keymaster, will connect to the remote keymaster server to authenticate and get an
   * identity (master token)
   * @param service Keymaster service
   */
  case class KeymasterIdentityProvider(service: Service[Request, Response])
      extends IdentityProvider[Credential, Tokens] {
    val endpoint = "/api/auth/service/v1/account_master_token"

    def api(cred: Credential): Request =
      RequestBuilder.create
          .addFormElement(("e", cred.email), ("p", cred.password), ("s", cred.serviceId.name))
          .url(endpoint)
          .buildFormPost()

    /**
     * Sends credentials, if authenticated successfully will return a MasterToken otherwise a Future.exception
     */
    def apply(req: IdentifyRequest[Credential]): Future[IdentifyResponse[Tokens]] =
      service(api(req.credential)).flatMap(res =>
        Tokens.derive[Tokens](res.contentString).fold[Future[IdentifyResponse[Tokens]]](
          err => Future.exception(err),
          t => Future.value(KeymasterIdentifyRes(t))
        )
      )
  }

  /**
   * Handles logins to the KeymasterIdentityProvider:
   * - saves the Tokens after a successful login
   * - sends the User to their original request location from before they logged in or the default location based on
   *   their service
   * @param store
   * @param secretStoreApi
   */
  case class KeymasterLoginFilter(store: sessionx.SessionStore)(implicit secretStoreApi: sessionx.SecretStoreApi)
      extends Filter[ServiceRequest, Response, IdentifyRequest[Credential], IdentifyResponse[Tokens]] {

    def createIdentifyReq(req: ServiceRequest): Option[IdentifyRequest[Credential]] =
      for {
        u <- req.req.params.get("username")
        p <- req.req.params.get("password")
      } yield KeymasterIdentifyReq(Credential(u, p, req.id))

    /**
     * Grab the original request from the session store, otherwise just send them to the default location of '/'
     */
    def originalRequestOrDefault(id: SessionId, req: ServiceRequest): Future[Request] =
      for {
        maybeReq <- store.get[Request](id)
      } yield maybeReq.fold(tap(Request("/"))(r => r.host = req.req.host.get))(_.data)

    def apply(req: ServiceRequest,
              service: Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]]): Future[Response] =
      createIdentifyReq(req).fold(Future.value(Response(Status.BadRequest)))(credReq =>
        for {
          sid <- SessionId.fromRequest(req.req).toFuture
          originReq <- originalRequestOrDefault(sid, req)
          tokenResponse <- service(credReq)
          session <- Session(tokenResponse.identity.id)
          _ <- store.update[Tokens](session)
          _ <- store.delete(sid)
        } yield tap(Response(Status.TemporaryRedirect))(res => {
          res.location = originReq.uri
          res.addCookie(session.id.asCookie)
        })
      )
  }

  /**
   * The access issuer will use the MasterToken to gain access to service tokens
   * @param service Keymaster service
   */
  case class KeymasterAccessIssuer(service: Service[Request, Response], store: sessionx.SessionStore)
      extends AccessIssuer[Tokens, ServiceToken] {
    val endpoint = "/api/auth/service/v1/account_token.json"

    def api(req: AccessRequest[Tokens]): Request =
      RequestBuilder.create
          .addHeader("Auth-Token", req.identity.id.master.value)
          .addFormElement(("services", req.serviceId.name))
          .url(endpoint)
          .buildFormPost()

    /**
     * Fetch a valid ServiceToken, will return a ServiceToken otherwise a Future.exception
     */
    def apply(req: AccessRequest[Tokens]): Future[AccessResponse[ServiceToken]]  =
      req.identity.id.service(req.serviceId.name).fold[Future[ServiceToken]](
        service(api(req)).flatMap(res =>
          Tokens.derive[Tokens](res.contentString).fold[Future[ServiceToken]](
            e => Future.exception(e),
            t => t.service(req.serviceId.name).fold[Future[ServiceToken]](
              Future.exception(AccessDenied)
            )(st => Future.value(st))
          )
        )
      )(t => Future.value(t)).map(t => KeymasterAccessRes(Access(t)))
  }

}
