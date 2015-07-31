/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Lookout, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.lookout.borderpatrol.sessionx

import java.util.concurrent.TimeUnit
import javax.crypto.spec.SecretKeySpec

import com.lookout.borderpatrol.sessionx.crypto.{Generator, Signer}
import com.twitter.bijection.Injection
import com.twitter.util.{Base64StringEncoder, Future, Time, Duration}

import scala.util.Try

trait SessionTypes {
  type Encrypted = Array[Byte]
  type Seconds = Long
  type Size = Int
  type TimeBytes = IndexedSeq[Byte]
  type Entropy = IndexedSeq[Byte]
  type SecretId = Byte
  type Payload = IndexedSeq[Byte]
  type Signature = IndexedSeq[Byte]

  /**
   * Creates a new [[Secret]] that can be used to [[Signer.sign]]
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
    private[sessionx] val idSize = 1
    private[sessionx] val lifetime = Duration(1, TimeUnit.DAYS)

    private[sessionx] def currentExpiry: Time =
      Time.now + lifetime

    private[sessionx] def id: SecretId =
      EntropyGenerator(idSize).head

    private[sessionx] def entropy: Entropy =
      EntropyGenerator(entropySize)

    /**
     * Creates a new [[Secret]] with default expiry of 1 day
     * @param expiry the time this should expire, defaults to 1 day
     * @return a new Secret
     */
    def apply(expiry: Time = currentExpiry): Secret =
      Secret(id, expiry, entropy)
  }


  /**
   * Place for current and previous valid [[Secret]] as they rotate
   *
   * @param current the current [[Secret]]
   * @param previous the previous (and potentially expired) [[Secret]]
   */
  case class Secrets(current: Secret, previous: Secret)

  /**
   * This type represents the value of a user's [[com.twitter.finagle.httpx.Cookie Cookie]]
   * The design of this is to act as a cryptographically verifiable identifier for a user
   *
   * In [[String]] form, is a `Base64` encoded concatenation of the following transformation:
   *  expires: [[Time]] => `Long` => `Array[Byte]`
   *  entropy: `Array[Byte]`
   *  secret: [[Secret]] => [[SecretId]]
   *  signature: `Array[Byte]`
   *
   * @param expires the time at which this id is expired
   * @param entropy the random bytes unique to this session id
   * @param secret the secret used to sign this session id
   * @param signature the bytes of the signature(expires, entropy, secret.id)
   */
  case class SessionId(expires: Time, entropy: Entropy, secret: Secret, signature: Signature)

  object SessionId {

    import Generator.EntropyGenerator

    val entropySize: Size = 16
    val lifetime = Duration(1, TimeUnit.DAYS)

    private[this] def currentExpiry: Time =
      Time.now + lifetime

    private[sessionx] def expired(t: Time): Boolean =
      t < Time.now || t > currentExpiry

    private[sessionx] def expired(id: SessionId): Boolean =
      expired(id.expires)

    private[sessionx] def genEntropy: Entropy =
      EntropyGenerator(entropySize)

    private[sessionx] def timeBytes(t: Time): TimeBytes =
      Injection.long2BigEndian(t.inMilliseconds)

    private[sessionx] def payload(t: Time, e: Entropy, i: SecretId): Payload =
      timeBytes(t) ++ e :+ i

    private[sessionx] def payload(id: SessionId): Payload =
      payload(id.expires, id.entropy, id.secret.id)

    private[sessionx] def signWith(id: SessionId, s: Secret): Signature =
      s.sign(payload(id))

    /**
     * Generate a new id wrapped in a [[Future]] since entropy is blocking on the JVM
     *
     * @param store where to fetch the current [[Secret]] to sign this id
     * @return
     */
    def next(implicit store: SecretStoreApi): Future[SessionId] =
      Future.value[SessionId](SessionId(currentExpiry, genEntropy, store.current))

    /**
     * Creates a new [[SessionId]] based on existing values
     *
     * @param expires
     * @param entropy
     * @param secret
     * @return
     */
    def apply(expires: Time, entropy: Entropy, secret: Secret): SessionId =
      new SessionId(expires, entropy, secret, secret.sign(payload(expires, entropy, secret.id)))

    /**
     * Converts into `A` given an implicit function `f: SessionId => A`
     * @param id
     * @param f
     * @tparam A
     * @return
     */
    def as[A](id: SessionId)(implicit f: SessionId => A): A =
      f(id)

    def from[A](a: A)(implicit f: A => Try[SessionId]): Try[SessionId] =
      f(a)

    implicit def toIndexedSeq(id: SessionId): IndexedSeq[Byte] =
      payload(id) ++ id.signature

    implicit def toArray(id: SessionId): Array[Byte] =
      toIndexedSeq(id).toArray

    implicit def toBase64(id: SessionId): String =
      Base64StringEncoder.encode(toArray(id))
  }

  trait SecretStoreApi {
    def current: Secret
    def previous: Secret
    def find(f: Secret => Boolean): Option[Secret]
  }

  abstract class BorderSessionError(val description: String) extends Exception
  class UpdateStoreException(description: String = "") extends BorderSessionError(description)
  class SessionIdException(description: String = "") extends BorderSessionError(description)
  class DecodeSessionJsonException(description: String = "") extends BorderSessionError(description)
}
