package com.lookout.borderpatrol.session

import argonaut._
import argonaut.Argonaut._

package object secret {

  implicit def SecretCodecJson: CodecJson[Secret] =
    casecodec3(Secret.apply, Secret.unapply)("expiry", "id", "entropy")

  implicit def SecretsCodecJson: CodecJson[Secrets] =
    casecodec2(Secrets.apply, Secrets.unapply)("current", "previous")

  implicit class SecretsJsonEncode(val ss: Secrets) extends AnyVal {
    def asJson: String =
      SecretsCodecJson.encode(ss).toString
  }

  implicit class SecretsJsonDecode(val s: String) extends AnyVal {
    def asSecrets: Option[Secrets] =
      s.decodeOption[Secrets]
  }

  implicit def SecretDataCodecJson: CodecJson[SecretData] =
    casecodec6(SecretData.apply, SecretData.unapply)("CreateIndex", "ModifyIndex", "LockIndex", "Key", "Flags", "Value")
}