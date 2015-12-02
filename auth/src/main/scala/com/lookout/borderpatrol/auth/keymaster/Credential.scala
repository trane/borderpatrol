package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.ServiceIdentifier
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.finagle.httpx.{Method, Request}


trait Credential {
  val serviceId: ServiceIdentifier
  val uniqueId: String
  def toRequest: Request
}

case class InternalAuthCredential(uniqueId: String, password: String, serviceId: ServiceIdentifier) extends Credential {
  def toRequest: Request =
    tap(Request(Method.Post, serviceId.loginManager.identityManager.path.toString))(req => {
      req.contentType = "application/x-www-form-urlencoded"
      req.contentString = Request.queryString(("s", serviceId.name), ("email", uniqueId), ("password", password))
        .drop(1) /* Drop '?' */
    })
}

case class OAuth2CodeCredential(uniqueId: String, subject: String, serviceId: ServiceIdentifier)
  extends Credential {
  def toRequest: Request =
    tap(Request(Method.Post, serviceId.loginManager.identityManager.path.toString))(req => {
      req.contentType = "application/x-www-form-urlencoded"
      req.contentString = Request.queryString(("s", serviceId.name), ("external_id", subject),
        ("ident_provider", serviceId.loginManager.name), ("enterprise", serviceId.subdomain))
        .drop(1) /* Drop '?' */
    })
}
