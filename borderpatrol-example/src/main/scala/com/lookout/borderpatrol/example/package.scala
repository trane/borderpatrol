package com.lookout.borderpatrol

import com.twitter.finagle.httpx

package object example {
  import auth._
  import session.SessionId

  case class Session(id: SessionId, request: httpx.Request, data: AuthInfo[Basic]) extends SecureSession[httpx.Request, AuthInfo[Basic]]

}
