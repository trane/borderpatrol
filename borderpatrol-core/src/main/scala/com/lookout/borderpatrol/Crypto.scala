package com.lookout.borderpatrol

import java.security.{Key, SecureRandom}
import java.util.concurrent.TimeUnit
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.twitter.util.{Duration, Time}

import scala.util.Random

trait Generator
object Generator extends Generator {
  private[this] val random = new Random(new SecureRandom)

  private[this] def nextByte = random.nextInt.toByte

  def apply(n: Int): Array[Byte] = data(n)
  def data(n: Int): Array[Byte] = Array.fill(n)(nextByte)
}

trait Signer {
  val algo: String
  def key: Key
  def hmac: Mac = { val m = Mac.getInstance(algo); m.init(key); m }
  def sign(bytes: Array[Byte]): Array[Byte] = hmac.doFinal(bytes)
}

sealed trait Secret extends Signer {
  val algo = "HmacSHA256"
  val dataSize = 16
  def key = new SecretKeySpec(Generator.data(dataSize), algo)
  def valid = expiry < Time.now
  val expiry: Time
}

case class Current(expiry: Time) extends Secret
case class Previous(current: Current) extends Secret {
  override def key = current.key
  val expiry = current.expiry
}

case class Secrets(current: Current, previous: Option[Previous])

trait Expiry {
  val lifetime: Duration
  def currentExpiry: Time = Time.now + lifetime
}

/**
 * This prototypes out an API for the SecretStore, keeping secrets in memory
 * which obviously doesn't work in a multi-server environment.
 *
 * Further work should be done to coordinate secrets among processes.
 *
 * For example, a zookeeper watcher could update current and previous in memory
 * on change, while an external service handles writing new secrets.
 */
object SecretStore extends Expiry {
  val lifetime: Duration = Duration(1, TimeUnit.DAYS)

  private[this] var _secrets: Secrets = Secrets(Current(currentExpiry), None)


  def current: Current = {
    val c = _secrets.current
    if (c.valid) c
    else {
      val c2 = Current(currentExpiry)
      _secrets = Secrets(c2, Some(Previous(c2)))
      c2
    }
  }

  def previous: Option[Previous] = _secrets.previous

  def find(f: (Secret) => Boolean): Option[Secret] =
    if (f(current)) Some(current)
    else previous match {
      case Some(p) if f(p) => Some(p)
      case _ => None
    }
}

