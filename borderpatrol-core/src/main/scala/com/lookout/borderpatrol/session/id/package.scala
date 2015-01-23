package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._

import scala.util.Try

package object id {

  implicit def SessionIdCodecJson: CodecJson[SessionId] =
    casecodec4(SessionId.apply, SessionId.unapply)("expires", "entropy", "secretId", "signature")

  implicit class SessionIdSerialize(val s: SessionId) extends AnyVal {
    def asString(implicit marshaller: Marshaller): String =
      marshaller.encode(s)
  }

  implicit class SessionIdDeserialize(val s: String) extends AnyVal {
    def asSessionId(implicit marshaller: Marshaller): Try[SessionId] =
      marshaller.decode(s)
  }

  implicit class SessionIdAndSecretDeserialize(val s: String) extends AnyVal {
    def asSessionIdAndSecret(implicit marshaller: Marshaller): Try[(SessionId, Secret)] =
      marshaller.decodeWithSecret(s)
  }

  implicit class SessionIdAndSecret(val s: SessionId) extends AnyVal {
    def asSessionIdAndSecret(implicit marshaller: Marshaller): Try[(SessionId, Secret)] =
      marshaller.injector.idAndSecret2Id.invert(s)
  }
}
