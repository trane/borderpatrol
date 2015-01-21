package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit
import javax.crypto.spec.SecretKeySpec

import com.twitter.util.{Duration, Time}

object SecretExpiry extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

sealed trait ASecret extends Signer {
  val algo = "HmacSHA256"
  val entropySize = Constants.Secret.entropySize
  val id: Byte
  val entropy: List[Byte]

  val expiry: Time
  lazy val key = new SecretKeySpec(entropy.toArray, algo)
}

case class Secret(expiry: Time,
                  id: Byte = Generator(1).head,
                  entropy: List[Byte] = Generator(16).toList) extends ASecret

case class Secrets(current: Secret, previous: Secret)