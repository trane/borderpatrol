package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.{CustomerIdentifier, ServiceIdentifier}
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.finagle.http.{Method, Request}


trait Credential {
  val uniqueId: String
  val customerId: CustomerIdentifier
  val serviceId: ServiceIdentifier
  def toRequest: Request
}

case class InternalAuthCredential(uniqueId: String, password: String, customerId: CustomerIdentifier,
                                  serviceId: ServiceIdentifier)
    extends Credential {
  def toRequest: Request =
    tap(Request(Method.Post, customerId.loginManager.identityManager.path.toString))(req => {
      req.contentType = "application/x-www-form-urlencoded"
      req.contentString = Request.queryString(("s", serviceId.name), ("email", uniqueId), ("password", password))
        .drop(1) /* Drop '?' */
    })
}

case class OAuth2CodeCredential(uniqueId: String, subject: String, customerId: CustomerIdentifier,
                                serviceId: ServiceIdentifier)
    extends Credential {
  def toRequest: Request =
    tap(Request(Method.Post, customerId.loginManager.identityManager.path.toString))(req => {
      req.contentType = "application/x-www-form-urlencoded"
      req.contentString = Request.queryString(("s", serviceId.name), ("external_id", subject),
        ("ident_provider", customerId.loginManager.name), ("enterprise", customerId.subdomain))
        .drop(1) /* Drop '?' */
    })
}
