package com.lookout.borderpatrol.session

import java.security.{Key, SecureRandom}
import javax.crypto.Mac

import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.util.{Duration, Time}

import scala.util.Random

trait RandomGenerator {
  val random: Random
}

object Generator extends RandomGenerator {
  val random = new Random(new SecureRandom)

  def apply(n: Int): Seq[Byte] =
    tap(Array.fill[Byte](n)(0))(random.nextBytes)
}

trait Expiry {
  val lifetime: Duration
  def currentExpiry: Time =
    Time.now + lifetime
}

trait Signer {
  val algo: String
  val key: Key
  lazy val hmac: Mac = tap(Mac.getInstance(algo))(mac => mac.init(key))

  def sign(bytes: Seq[Byte]): Seq[Byte] =
    hmac.doFinal(bytes.toArray)
}
