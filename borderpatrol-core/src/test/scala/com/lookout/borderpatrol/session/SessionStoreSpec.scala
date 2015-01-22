package com.lookout.borderpatrol.session

import org.jboss.netty.handler.codec.http.{HttpRequest, DefaultHttpRequest, HttpMethod, HttpVersion}
import org.scalatest.{FlatSpec, Matchers}

class SessionStoreSpec extends FlatSpec with Matchers {
  import org.scalatest.OptionValues._
  import Session._

  def defaultReq: HttpRequest = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")

  behavior of "InMemoryEncryptedSessionStore"
  val encryptedStore = new InMemoryEncryptedSessionStore

  it should "store and retrieve sessions" in {
    val s = Session("service", defaultReq)
    encryptedStore.update(s)
    encryptedStore.get(s.id).value.equals(s) shouldBe true
  }

  it should "create a CryptKey for a session" in {
    val s = Session("service", defaultReq)
    encryptedStore.cryptKey(s.id).isSuccess shouldBe true
    encryptedStore.cryptKey(s.id.asString).isSuccess shouldBe true
  }

  it should "fail to create a CryptKey for an invalid session" in {
    val s = Session("service", defaultReq)
    val invalidSecretId = ~s.id.secretId
    val id = SessionId(s.id.expires, s.id.entropy, invalidSecretId.toByte, s.id.signature)
    encryptedStore.cryptKey(id).isFailure shouldBe true
    encryptedStore.cryptKey(id.asString).isFailure shouldBe true
  }
}
