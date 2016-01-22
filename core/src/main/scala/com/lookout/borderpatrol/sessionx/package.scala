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

package com.lookout.borderpatrol

import argonaut._, Argonaut._
import com.twitter.finagle.http.{Request, Cookie}
import com.twitter.util.{Time, Future}

import scala.util.{Failure, Success}

/**
 * This introduces types and functions that enable identifying, fetching, and storing web session data. This
 * is accomplished by a set of types that will be used by consumers of this library: `Session`, `Store`, and `Secret`.
 *
 * A [[com.lookout.borderpatrol.sessionx.Secret Secret]] is a cryptographically verifiable signing key used to sign a
 * [[com.lookout.borderpatrol.sessionx.SignedId SignedId]]. Creating a `Secret` is simple. It defaults to expire at
 * [[com.lookout.borderpatrol.sessionx.Secret.lifetime Secret.lifetime]]
 *
 * {{{
 *   val secret = Secret() // default secret expiry
 *   val expiringSecret = Secret(Time.now)
 *
 *   val randomBytes = EntropyGenerator(16) // 16 bytes of randomness
 *   val randomId = EntropyGenerator(1).head // 1 byte of randomness for an id
 *   val expiry = Time.from(0) // very expired
 *   val constructedSecret = Secret(randomId, randomBytes, expiry)
 *   log(s"secret has expired: ${constructedSecret.expired == true}")
 *
 *   val signedMsg = secret.sign("message to by signed".getBytes)
 * }}}
 *
 * A [[com.lookout.borderpatrol.sessionx.SignedId SignedId]] is a cryptographically signed identifier for a
 * [[com.lookout.borderpatrol.sessionx.Session Session]], it consists of entropy, expiry, secret,and signature of those
 * items. This is meant to be used as the [[com.twitter.finagle.http.Cookie]] value, so we provide serializing to
 * [[String]].
 *
 * {{{
 *   val id: SignedId = Await.result(SignedId.next)
 *   val cookieValue: String = id.asBase64
 *   SignedId.from[String](cookieValue) == id
 * }}}
 *
 * A [[com.lookout.borderpatrol.sessionx.Session Session]] is product type of a cryptographically verifiable
 * identifier [[com.lookout.borderpatrol.sessionx.SignedId SignedId]] and an arbitrary data type
 * A`. The only requirement for a [[com.lookout.borderpatrol.sessionx.SessionStore SessionStore]][B,M] to store/fetch
 * a `Session[A]` is that there be some implicit injective views from `A => B` and `B => Try[A]`.
 *
 * We have provided default encodings for: `http.Request => Buf`, `String => Buf` and their injective views.
 *
 * {{{
 *  // set up secret/session stores
 *  implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
 *  val sessionStore = SessionStore.InMemoryStore()
 *
 *  // create a Session[http.Request]
 *  val newSessionFuture = Session(Request("http://localhost/api/stuff")) // entropy is blocking on the JVM
 *  val newSession = Await.result(newSessionFuture)
 *
 *  // see if the session expired (checks the [[SignedId.expires]])
 *  log(s"Session has expired? ${newSession.expired}")
 *
 *  // store the session and then fetch it
 *  sessionStore.update(newSession).onFailure(log)
 *  sessionStore.get(newSession.id).onSuccess(s => s match {
 *    case Some(s) => log(s"Same session?: ${newSession == s}")
 *    case None => log("hrm, where did the session go?")
 *  })
 * }}}
 *
 * Let's say you have a [[com.lookout.borderpatrol.sessionx.Session.data Session.data]] type that doesn't have the
 * injective that you need, that's OK!
 * Assuming you are storing it in memcached, which requires a type of [[com.twitter.io.Buf Buf]] for the value:
 *
 * {{{
 *   trait Foo {
 *     val value: Int
 *   }
 *
 *   implicit val enc = SessionDataEncoder[Foo](
 *     foo => Buf.U32BE(foo.value),
 *     buf => new Foo { override val value = Buf.U32BE.unapply(buf) }
 *   )
 *
 *   val foo1 = new Foo { override val value = 1 }
 *   val fooSession = Session(foo1)
 *   sessionStore.update(fooSession)
 * }}}
 *
 */
package object sessionx extends Types {

  /**
   * Wraps any object with a `toFuture` method
   */
  implicit class AnyOps[A](val any: A) extends AnyVal {

    /**
     * Wraps object into `Future`
     */
    def toFuture: Future[A] = Future.value[A](any)
  }

  /**
   * Wraps any `Throwable` with a `toFutureException` method
   */
  implicit class ThrowableOps(val t: Throwable) extends AnyVal {

    /**
     * Wraps `Throwable` in a `Future` exception
     */
    def toFutureException[A]: Future[A] = Future.exception[A](t)
  }

  implicit class SecretOps(val s: Secret) extends AnyVal {
    def expired: Boolean =
      s.expiry < Time.now || s.expiry > Secret.currentExpiry
  }

  /**
   * More object-style accessors on SignedId, implementation defined in
   * [[com.lookout.borderpatrol.sessionx.SignedId SignedId]]
   */
  implicit class SignedIdOps(val id: SignedId) extends AnyVal {
    def signWith(s: Secret): Signature =
      SignedId.signWith(id, s)

    def expired: Boolean =
      SignedId.expired(id)

    def asBase64: String =
      SignedId.toBase64(id)

    def asCookie(cookieName: String = SignedId.sessionIdCookieName): Cookie =
      SignedId.toCookie(id, cookieName)
  }

  /**
   * Helpful value class for Session operations
   */
  implicit class SessionOps[A](val s: Session[A]) extends AnyVal {
    def expired: Boolean =
      s.id.expired
  }

  /**
   * Helper for Byte -> Json
   */
  implicit val ByteCodecJson: CodecJson[Byte] =
    CodecJson(
      (b: Byte) => jNumberOrNull(b.toInt),
      c => for (b <- c.as[Int]) yield b.toByte
    )

  /**
   * Time -> Long -> Json
   */
  implicit val TimeCodecJson: CodecJson[Time] =
    CodecJson(
      (t: Time) =>
        ("ms" := t.inMilliseconds) ->: jEmptyObject,
      c => for {
        s <- (c --\ "ms").as[Long]
      } yield Time.fromMilliseconds(s))

}
