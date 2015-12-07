package com.lookout.borderpatrol.security

import java.net.InetAddress

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.httpx.{HeaderMap, Request, Response}
import com.twitter.util.Future
import com.lookout.borderpatrol.util.Combinators.tap

/**
  * Basic, default values for adding security mechanisms via headers
  */
object Headers {
  val StrictTransportSecurity = ("Strict-Transport-Security", "max-age=31557600")
  val XFrameOptions = ("X-Frame-Options", "DENY")
  val XXSSProtection = ("X-XSS-Protection", "1; mode=block")
  val XContentTypeOptions = ("X-ContentType-Options", "nosniff")
  val XDownloadOptions = ("X-Download-Options", "noopen")
  val XPermittedCrossDomainPolicies = ("X-Permitted-Cross-Domain-Policies", "none")

  val all = HeaderMap(StrictTransportSecurity, XFrameOptions, XXSSProtection,
                      XContentTypeOptions, XDownloadOptions, XPermittedCrossDomainPolicies)
}

/**
  * Inject specific headers into requests for added security or auditing
  *
  * By default it includes the following default headers and values:
  *
  *   Strict-Transport-Security: max-age=31557600
  *   X-Frame-Options: DENY
  *   X-XSS-Protection: 1; mode=block
  *   X-ContentType-Options: nosniff
  *   X-Download-Options: noopen
  *   X-Permitted-Cross-Domain-Policies: none
  *   X-Forwarded-For: the ip of this host appended to any existing values
  *
  * @param headers
  */
case class HeaderFilter(headers: HeaderMap = Headers.all) extends SimpleFilter[Request, Response] {
  val localIp = InetAddress.getLocalHost.getHostAddress

  def apply(req: Request, service: Service[Request, Response]): Future[Response] =
    service(tap(req) { r =>
        r.xForwardedFor = (r.xForwardedFor match {
          case Some(s) => s"$s, $localIp"
          case None => localIp
        })
      r.headerMap ++= headers
    })
}
