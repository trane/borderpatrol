package com.lookout.borderpatrol.session

import java.security.{Key, SecureRandom}
import java.util.concurrent.TimeUnit
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.util.{Duration, Time}

import scala.util.{Failure, Random, Success, Try}

trait Generator
object Generator extends Generator {

  private[this] val random = new Random(new SecureRandom)

  def apply(n: Int): Array[Byte] = tap(Array.fill[Byte](n)(0))(random.nextBytes)
}

trait Expiry {
  val lifetime: Duration
  def currentExpiry: Time = Time.now + lifetime
}

object SecretExpiry extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

trait Signer {
  val algo: String
  val key: Key
  lazy val hmac: Mac = tap(Mac.getInstance(algo))(mac => mac.init(key))
  def sign(bytes: Array[Byte]): Array[Byte] = hmac.doFinal(bytes)
}

sealed trait Secret extends Signer {
  val algo = "HmacSHA256"
  val entropySize = Constants.Secret.entropySize
  val id: Byte = Generator(1).head

  val expiry: Time
}

case class Current(expiry: Time) extends Secret {
  val key = new SecretKeySpec(Generator(entropySize), algo)
}

case class Previous(current: Current) extends Secret {
  val key = current.key
  val expiry = current.expiry
}

case class Secrets(current: Current, previous: Option[Previous])

/**
 * This prototypes out an API for the SecretStore, keeping secrets in memory
 * which obviously doesn't work in a multi-server environment.
 *
 * Further work should be done to coordinate secrets among processes.
 *
 * For example, a zookeeper watcher could update current and previous in memory
 * on change, while an external service handles writing new secrets.
 */
sealed trait SecretStoreApi {
  def current: Current
  def previous: Option[Previous]
  def find(f: (Secret) => Boolean): Try[Secret]
}

case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
  import SecretExpiry._
  private[this] var _secrets: Secrets = secrets

  def current = {
    val c = _secrets.current
    if (c.expiry > Time.now && c.expiry <= currentExpiry) c
    else {
      val c2 = Current(currentExpiry)
      _secrets = Secrets(c2, Some(Previous(c)))
      c2
    }
  }

  def previous = _secrets.previous

  def find(f: (Secret) => Boolean) =
    if (f(current)) Success(current)
    else previous match {
      case Some(p) if f(p) => Success(p)
      case _ => Failure(new Exception("No matching secrets found"))
    }
}

trait SecretStoreComponent {
  implicit val secretStore: SecretStoreApi
}
