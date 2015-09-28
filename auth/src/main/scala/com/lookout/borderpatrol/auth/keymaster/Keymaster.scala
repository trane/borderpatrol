package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.{sessionx, ServiceIdentifier}
import com.lookout.borderpatrol.auth._
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.{RequestBuilder, Response, Request}
import com.twitter.util.Future

object Keymaster {
  case class Credential(email: String, password: String, serviceId: ServiceIdentifier)

  class IdentifyException extends Exception("")
  case object AccessDenied extends Exception("no access available for this service")

  case class KeymasterIdentifyReq(credential: Credential) extends IdentifyRequest[Credential]
  case class KeymasterIdentifyRes(tokens: Tokens) extends IdentifyResponse[Tokens] {
    val identity = Identity(tokens)
  }
  case class KeymasterAccessReq(identity: Id[Token], serviceId: ServiceIdentifier) extends AccessRequest[Token]
  case class KeymasterAccessRes(access: Access[ServiceToken]) extends AccessResponse[ServiceToken]

  /**
   * The identity provider for Keymaster, will connect to the remote keymaster server to authenticate and get an
   * identity (master token)
   * @param service Keymaster service
   */
  case class KeymasterIdentityProvider(service: Service[Request, Response], store: sessionx.SessionStore)
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
   * The access issuer will use the MasterToken to gain access to service tokens
   * @param service Keymaster service
   */
  case class KeymasterAccessIssuer(service: Service[Request, Response], store: sessionx.SessionStore)
      extends AccessIssuer[MasterToken, ServiceToken] {
    val endpoint = "/api/auth/service/v1/account_token.json"

    def api(req: AccessRequest[MasterToken]): Request =
      RequestBuilder.create
          .addHeader("Auth-Token", req.identity.id.value)
          .addFormElement(("services", req.serviceId.name))
          .url(endpoint)
          .buildFormPost()

    /**
     * Sends MasterToken, will return a ServiceToken otherwise a Future.exception
     */
    def apply(req: AccessRequest[MasterToken]): Future[AccessResponse[ServiceToken]]  =
      service(api(req)).flatMap(res =>
        Tokens.derive[Tokens](res.contentString).fold[Future[AccessResponse[ServiceToken]]](
          e => Future.exception(e),
          t => t.service(req.serviceId.name).fold[Future[AccessResponse[ServiceToken]]](
            Future.exception(AccessDenied)
          )(st => Future.value(KeymasterAccessRes(Access(st))))
        )
      )
  }

}
