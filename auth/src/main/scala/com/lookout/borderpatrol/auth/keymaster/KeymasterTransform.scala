package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.Transform
import com.lookout.borderpatrol.auth.{IdentityProviderError, SessionIdRequest}
import com.twitter.finagle.httpx.Status
import com.twitter.util.Future

object KeymasterTransform {
  import Tokens._

  implicit val transformBasic: Transform[SessionIdRequest, Future[InternalAuthCredential]] =
    new Transform[SessionIdRequest, Future[InternalAuthCredential]] {
      def apply(req: SessionIdRequest): Future[InternalAuthCredential] = {
        (for {
          u <- req.req.req.params.get("username")
          p <- req.req.req.params.get("password")
        } yield InternalAuthCredential(u, p, req.req.serviceId)) match {
          case Some(c) => Future.value(c)
          case None => Future.exception(IdentityProviderError(Status.InternalServerError,
            "transformBasic: Failed to parse the Request"))
        }
      }
    }

  implicit val transformOAuth2: Transform[SessionIdRequest, Future[OAuth2CodeCredential]] =
    new Transform[SessionIdRequest, Future[OAuth2CodeCredential]] {
      def apply(req: SessionIdRequest): Future[OAuth2CodeCredential] = {
        for {
          aadToken <- req.req.serviceId.loginManager.protoManager.codeToToken(
            req.req.req.host, req.req.req.getParam("code")).flatMap(res => res.status match {
              //  Parse for Tokens if Status.Ok
              case Status.Ok =>
                Tokens.derive[AadToken](res.contentString).fold[Future[AadToken]](
                  err => Future.exception(IdentityProviderError(Status.InternalServerError,
                    "Failed to parse the AadToken received from OAuth2 Server: " +
                      s"${req.req.serviceId.loginManager.name}")),
                  t => Future.value(t)
                )
              //  Preserve Response Status code by throwing AccessDenied exceptions
              case _ => Future.exception(IdentityProviderError(res.status,
                s"Failed to receive the AadToken from OAuth2 Server: ${req.req.serviceId.loginManager.name}"))
            })
          idClaimSet <- Tokens.JwtParse(aadToken.idToken)
        } yield OAuth2CodeCredential(idClaimSet.getStringClaim("upn"), idClaimSet.getSubject, req.req.serviceId)
      }
    }
}
