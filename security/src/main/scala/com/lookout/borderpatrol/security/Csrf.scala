package com.lookout.borderpatrol.security

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.{SimpleFilter, Service, Filter}
import com.twitter.finagle.http.{Status, Cookie, Request, Response}
import com.twitter.util.{Time, Future}

object Csrf {
  case class InHeader(val header: String = "X-BORDER-CSRF") extends AnyVal
  case class Param(val param: String = "_x_border_csrf") extends AnyVal
  case class CookieName(val name: String = "border_csrf") extends AnyVal
  case class VerifiedHeader(val header: String = "X-BORDER-CSRF-VERIFIED") extends AnyVal

  /**
   * Informs upstream service about Csrf validation via double submit cookie
   *
   * @param header The incoming header that contains the CSRF token
   * @param param The incoming parameter that contains the CSRF token
   * @param cookieName The cookie that contains the CSRF token
   * @param verifiedHeader The verified header to set
   */
  case class Verify(header: InHeader,
                    param: Param,
                    cookieName: CookieName,
                    verifiedHeader: VerifiedHeader)(implicit secretStoreApi: SecretStoreApi) {

    /**
     * Inject the value of the call to verify in the VerifiedHeader
     * It's unsafe, because it mutates the Request
     */
    def unsafeInject(req: Request)(f: Boolean => String): Request =
      tap(req)(_.headerMap.set(verifiedHeader.header, f(verify(req))))

    /**
     * Check that CSRF header/param is there, validates that the cookie and header/param are valid SessionIds
     * If the header is not present it will look for the parameter.
     * @return false unless all checks are valid
     */
    def verify(req: Request): Boolean =
      (for {
        str <- req.headerMap.get(header.header) orElse req.params.get(param.param)
        uid <- SignedId.from(str).toOption
        cid <- SignedId.fromRequest(req, cookieName.name).toOption
      } yield uid == cid) getOrElse false
  }
}

/**
 * Inserts the CSRF cookie in a Response sent back to the client
 *
 * - It should be typically happen only once, perhaps after the successful login
 */
case class CsrfInsertFilter[A](cookieName: Csrf.CookieName)(implicit secretStore: SecretStoreApi)
    extends Filter[A, Response, A, Response] {

  def apply(req: A, service: Service[A, Response]): Future[Response] =
    service(req).flatMap(res =>
      for {
        csrfId <- SignedId.authenticated
        _ <- res.addCookie(csrfId.asCookie(cookieName.name)).toFuture
      } yield res
    )
}

/**
 * Sets the CSRF header for inspection by upstream service. Always sets csrf verified header to false unless
 * the csrf cookie and the header/param match and are valid.
 */
case class CsrfVerifyFilter(verify: Csrf.Verify) extends SimpleFilter[Request, Response] {

  def apply(req: Request, service: Service[Request, Response]): Future[Response] =
    service(verify.unsafeInject(req)(_.toString))
}
