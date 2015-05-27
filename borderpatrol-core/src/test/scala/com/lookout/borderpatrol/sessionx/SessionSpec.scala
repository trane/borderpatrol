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


import com.twitter.io.Buf
import com.twitter.util.{Await, Duration, Time}
import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Try, Failure, Success}

class SessionSpec extends FlatSpec with Matchers {

  behavior of "Secret"

  it should "have a default constructor with an expected expiry" in {
    Secret().expiry moreOrLessEquals(Secret.currentExpiry, Duration(1, TimeUnit.SECONDS))
  }

  it should "be unique on every construction" in {
    Secret().entropy should not equal Secret().entropy
  }

  it should "not be expired when newly created" in {
    Secret().expired shouldBe false
  }

  it should "be expired when expiry is < now" in {
    val oneSecondAgo = Time.now.minus(Duration(1, TimeUnit.SECONDS))
    Secret(oneSecondAgo).expired shouldBe true
  }

  it should "sign sequences of bytes" in {
    val seq = Vector[Byte](0, 1, 2)
    val s = Secret()
    val sig = s.sign(seq)
    sig shouldEqual s.sign(seq)
    sig should not equal Secret().sign(seq)
  }

  behavior of "Secrets"

  val current = Secret()
  val previous = Secret(Time.fromMilliseconds(0))
  val mockSecrets = Secrets(current, previous)

  it should "give the current and previous Secret" in {
    mockSecrets.current shouldEqual current
    mockSecrets.previous shouldEqual previous
  }

  behavior of "SecretStoreApi"

  it should "give the current and previous Secret" in {
    val secretStore = SecretStores.InMemorySecretStore(mockSecrets)
    secretStore.current shouldBe current
    secretStore.previous shouldBe previous
  }

  it should "always give a non-expired current secret" in {
    val secretStore = SecretStores.InMemorySecretStore(Secrets(previous, previous))
    secretStore.current should not be previous
    secretStore.current.expired shouldBe false
  }

  it should "find secrets if they exist" in {
    val secretStore = SecretStores.InMemorySecretStore(mockSecrets)
    secretStore.find(_.id == previous.id) shouldBe Some(previous)
    secretStore.find(_.id == current.id) shouldBe Some(current)
    secretStore.find(s => s.id != current.id && s.id != previous.id) shouldBe None
  }

  behavior of "SessionId"
  implicit val secretStore = SecretStores.InMemorySecretStore(mockSecrets)
  val id = SessionId.next
  val ent = Crypto.Generator.EntropyGenerator(16)
  val sig = current.sign(ent)
  val expiredId = SessionId(Time.fromMilliseconds(0), ent, current, sig)

  it should "create a sessionId with expiry" in {
    id.expired should not be true
    expiredId.expired shouldBe true
  }

  it should "be convertable to a string" in {
    implicit def str2Id(s: String) =
      SessionIdInjections.str2SessionId(s)
    val str = id.asBase64
    SessionId.from[String](str) shouldBe Success(id)
  }

  behavior of "Session"

  case class StringSession(id: SessionId, request: String, data: String) extends Session[String, String]
  case class IntSession(id: SessionId, request: Int, data: Int) extends Session[Int, Int]
  case class IntStringSession(id: SessionId, request: Int, data: String) extends Session[Int, String]

  implicit def s2str(s: Session[String, String]): String =
    s"${s.data}:${s.request}:${s.id.asBase64}}"
  implicit def s2arr(s: Session[String, String]): Seq[Byte] =
    s2str(s).getBytes()
  implicit def str2Id(s: String): Try[SessionId] =
    SessionIdInjections.str2SessionId(s)
  implicit def str2s(s: String): Try[Session[String, String]] =
    s.split(":").toList match {
      case List(data: String, req: String, id: String) => SessionId.from[String](id) map (Session(_, req, data))
      case _ => Failure(new Exception("failed"))
    }

  it should "expire" in {
    IntSession(id, 1, 1).expired shouldBe false
    IntSession(expiredId, 1, 1).expired shouldBe true
  }

  it should "be convertable" in {
    val session = StringSession(SessionId.next, "hello", "world")
    val str = session.as[String]
    val arr = session.as[Seq[Byte]]
    val session2 = Session.from[String](str).get

    str shouldBe a [String]
    arr shouldBe an [Seq[_]]

    session.id shouldEqual session2.id
  }

  behavior of "SessionStore"

  implicit def sint2str(s: Session[Int, String]): Buf =
    Buf.Utf8(s"${s.data}:${s.request}:${s.id.asBase64}}")
  implicit def id2str(id: SessionId): String =
    id.asBase64
  implicit def buf2Session(b: Buf): Option[Session[String, String]] = for {
    str <- Buf.Utf8.unapply(b)
    ses <- str2s(str).toOption
  } yield ses
  implicit val sessionStore = SessionStores.InMemoryStore()

  it should "store any type of session" in {
    val intSession = IntSession(SessionId.next, 1, 1)
    val strSession = IntStringSession(SessionId.next, 1, "Hi")
    sessionStore.update(intSession.id)(intSession)
    sessionStore.update(strSession.id)(strSession)
    Await.result(sessionStore get strSession.id) shouldBe Some(strSession)
    Await.result(sessionStore get intSession.id) shouldBe Some(intSession)
    Await.result(sessionStore get SessionId.next) shouldBe None
  }
}
