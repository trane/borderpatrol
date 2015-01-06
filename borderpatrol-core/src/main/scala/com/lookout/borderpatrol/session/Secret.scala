package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit
import javax.crypto.spec.SecretKeySpec

import com.twitter.util.{Duration, Time}

object SecretExpiry extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

sealed trait Secret extends Signer {
  val algo = "HmacSHA256"
  val entropySize = Constants.Secret.entropySize
  val id: Byte = Generator(1).head

  val expiry: Time
}

case class Current(expiry: Time) extends Secret {
  val key = new SecretKeySpec(Generator(entropySize).toArray, algo)
}

case class Previous(current: Current) extends Secret {
  val key = current.key
  val expiry = current.expiry
}

case class Secrets(current: Current, previous: Option[Previous])
