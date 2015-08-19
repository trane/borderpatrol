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

import com.twitter.util.{Await, Duration, Time}
import com.twitter.finagle.{httpx,memcachedx}
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Success

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
  val id = Await.result(SessionId.next)
  val ent = crypto.Generator.EntropyGenerator(16)
  val sig = current.sign(ent)
  val expiredId = SessionId(Time.fromMilliseconds(0), ent, current, sig)

  it should "create a sessionId with expiry" in {
    id.expired should not be true
    expiredId.expired shouldBe true
  }

  it should "be convertable to a string" in {
    val str = id.asBase64
    SessionId.from[String](str) shouldBe Success(id)
  }

  behavior of "Session"

  it should "expire" in {
    Session(id, 1).expired shouldBe false
    Session(expiredId, 1).expired shouldBe true
  }

  it should "be the same object in memory when equal" in {
    val id = Await.result(SessionId.next)
    Session(id, "session") shouldBe Session(id, "session")
    Session(id, "session1") should not be Session(id, "session2")

    Await.result(Session(1)) should not be Await.result(Session(1))
  }

  behavior of "SessionStore"

  val sessionStore = SessionStore.InMemoryStore
  val memcachedSessionStore = SessionStore.MemcachedStore(new memcachedx.MockClient())

  it should "store any type of session" in {
    val intSession = Session(Await.result(SessionId.next), 1)
    val strSession = Session(Await.result(SessionId.next), "string")

    List(sessionStore, memcachedSessionStore).map { store =>
      store.update[Int](intSession)
      store.update[String](strSession)
      Await.result(store.get[String](strSession.id)).get.data shouldEqual strSession.data
      val a = Await.result(store.get[Int](strSession.id)).get // test string -> buf -> int
      Await.result(store.get[Int](intSession.id)).get.data shouldBe intSession.data
      Await.result(store.get[Int](Await.result(SessionId.next))) shouldBe None
    }
  }

  it should "store request sessions" in {
    List(sessionStore, memcachedSessionStore).map { store =>
      val reqSession = Session(Await.result(SessionId.next), httpx.Request("localhost:8080"))
      sessionStore.update(reqSession)
      val a = Await.result(sessionStore.get[httpx.Request](reqSession.id)).get
      a.data.uri shouldEqual reqSession.data.uri
    }
  }

}
