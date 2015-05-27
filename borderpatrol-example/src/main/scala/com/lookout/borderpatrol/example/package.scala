package com.lookout.borderpatrol
import com.lookout.borderpatrol.{AuthRequest => _}
import io.finch.HttpRequest

package object example {
  import sessionx._
  import model._

  case class AuthRequest(key: ApiKey, http: HttpRequest)

  implicit val loginReqEv = (req: LoginRequest) => req.request
  implicit val authReqEv = (req: AuthRequest) => req.http
  implicit val pSessionEv: PSession => ApiKeySession =
    (ps: PSession) => ps.asInstanceOf[ApiKeySession]
}
