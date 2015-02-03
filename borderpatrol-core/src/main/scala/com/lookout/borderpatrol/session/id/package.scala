package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._

import scala.util.Try

package object id {

  implicit def SessionIdCodecJson: CodecJson[SessionId] =
    casecodec4(SessionId.apply, SessionId.unapply)("expires", "entropy", "secretId", "signature")


  implicit class StringOpsSessionId(val s: String) extends AnyVal {
    def asSessionId(implicit marshaller: Marshaller): Try[SessionId] =
      marshaller.decode(s)

    def asSessionIdAndSecret(implicit marshaller: Marshaller): Try[(SessionId, Secret)] =
      marshaller.decodeWithSecret(s)

    def asIdSecretAndKey(implicit marshaller: Marshaller): Try[(SessionId, Secret, CryptKey)] =
      marshaller.decodeWithSecret(s).map(t =>
        (t._1, t._2, CryptKey(t._1, t._2))
      )
  }

  implicit class SessionIdOps(val s: SessionId) extends AnyVal {
    def asSessionIdAndSecret(implicit marshaller: Marshaller): Try[(SessionId, Secret)] =
      marshaller.injector.idAndSecret2Id.invert(s)

    def asString(implicit marshaller: Marshaller): String =
      marshaller.encode(s)

    def deriveCryptKey(implicit marshaller: Marshaller): Option[CryptKey] =
      marshaller.injector.idAndSecret2Id.invert(s).toOption map (t => CryptKey(s, t._2))
  }

}
