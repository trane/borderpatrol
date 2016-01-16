package com.lookout.borderpatrol.sessionx

import java.util.concurrent.TimeUnit
import javax.crypto.spec.SecretKeySpec
import com.lookout.borderpatrol.crypto.{Generator, Signer}
import com.twitter.util.{Duration, Time}

/**
 * Creates a new [[com.lookout.borderpatrol.sessionx.Secret Secret]] that can
 * be used to [[com.lookout.borderpatrol.crypto.Signer.sign Signer.sign]]
 *
 * @param id unique identifier for this secret
 * @param expiry how long this signer is valid
 * @param entropy the random bytes for this key
 */
case class Secret(id: SecretId, expiry: Time, entropy: Entropy) extends Signer {
  val algo = "HmacSHA256"
  lazy val key = new SecretKeySpec(entropy.toArray, algo)
}

/**
 * Helper object for defining defaults for entropy, id size, and expiry
 *
 * {{{
 *   val validSecret = Secret()
 *   val anotherSecret = Secret(Time.now + Duration(1, TimeUnit.HOURS)) // expires in an hour
 * }}}
 */
object Secret {
  import Generator.EntropyGenerator

  private[sessionx] val entropySize = 16
  private[sessionx] val idSize = 2
  private[sessionx] val lifetime = Duration(1, TimeUnit.DAYS)

  private[borderpatrol] def currentExpiry: Time =
    Time.now + lifetime

  private[sessionx] def id: SecretId =
    EntropyGenerator(idSize)

  private[sessionx] def entropy: Entropy =
    EntropyGenerator(entropySize)

  /**
   * Creates a new [[Secret]] with default expiry of 1 day (set by [[lifetime]])
   * @param expiry the time this should expire, defaults to 1 day
   * @return a new Secret
   */
  def apply(expiry: Time = currentExpiry): Secret =
    Secret(id, expiry, entropy)
}


/**
 * Place for current and previous valid [[com.lookout.borderpatrol.sessionx.Secret Secret]] as they rotate
 *
 * @param current the current [[com.lookout.borderpatrol.sessionx.Secret Secret]]
 * @param previous the previous (and potentially expired) [[com.lookout.borderpatrol.sessionx.Secret Secret]]
 */
case class Secrets(current: Secret, previous: Secret)

object Secrets {
  def fromCurrent(current: Secret): Secrets =
    Secrets(Secret(current.expiry + Secret.lifetime), current)
}
