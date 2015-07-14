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

import com.lookout.borderpatrol.sessionx.Crypto.{Generator, Signer}
import com.twitter.bijection.Injection
import com.twitter.util._

trait SessionTypes {
  type Encrypted = Array[Byte]
  type Seconds = Long
  type Size = Int
  type TimeBytes = IndexedSeq[Byte]
  type Entropy = IndexedSeq[Byte]
  type SecretId = Byte
  type Payload = IndexedSeq[Byte]
  type Signature = IndexedSeq[Byte]

  case class Secret(id: SecretId, expiry: Time, entropy: Entropy) extends Signer {
    val algo = "HmacSHA256"
    lazy val key = new SecretKeySpec(entropy.toArray, algo)
  }

  object Secret {
    import Generator.EntropyGenerator

    val entropySize = 16
    val idSize = 1
    val lifetime = Duration(1, TimeUnit.DAYS)

    def currentExpiry: Time =
      Time.now + lifetime

    def id: SecretId =
      EntropyGenerator(idSize).head

    def entropy: Entropy =
      EntropyGenerator(entropySize)

    def apply(expiry: Time = currentExpiry): Secret =
      Secret(id, expiry, entropy)
  }


  case class Secrets(current: Secret, previous: Secret)
  case class SessionId(expires: Time, entropy: Entropy, secret: Secret, signature: Signature)

  object SessionId {

    import Generator.EntropyGenerator

    val entropySize: Size = 16
    val lifetime = Duration(1, TimeUnit.DAYS)

    def currentExpiry: Time =
      Time.now + lifetime

    def expired(t: Time): Boolean =
      t < Time.now || t > currentExpiry

    def expired(id: SessionId): Boolean =
      expired(id.expires)

    def genEntropy: Entropy =
      EntropyGenerator(entropySize)

    def timeBytes(t: Time): TimeBytes =
      Injection.long2BigEndian(t.inMilliseconds)

    def payload(t: Time, e: Entropy, i: SecretId): Payload =
      timeBytes(t) ++ e :+ i

    def payload(id: SessionId): Payload =
      payload(id.expires, id.entropy, id.secret.id)

    def signWith(id: SessionId, s: Secret): Signature =
      s.sign(payload(id))

    def next(implicit store: SecretStoreApi): Future[SessionId] =
      Future.value[SessionId](SessionId(currentExpiry, genEntropy, store.current))

    def apply(expires: Time, entropy: Entropy, secret: Secret): SessionId =
      new SessionId(expires, entropy, secret, secret.sign(payload(expires, entropy, secret.id)))

    def as[A](id: SessionId)(implicit f: SessionId => A): A =
      f(id)

    def from[A](a: A)(implicit store: SecretStoreApi, f: A => Try[SessionId]): Try[SessionId] =
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
