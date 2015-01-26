package com.lookout.borderpatrol.session

import com.lookout.borderpatrol.Session
import com.lookout.borderpatrol.session.id.{Marshaller, Generator => IdGenerator}
import com.lookout.borderpatrol.session.secret.InMemorySecretStore
import com.lookout.borderpatrol.session.store.{MemcachedSessionStore, InMemoryEncryptedSessionStore}
import org.jboss.netty.handler.codec.http.{HttpRequest, DefaultHttpRequest, HttpMethod, HttpVersion}
import org.scalatest.{FlatSpec, Matchers}

class SessionStoreSpec extends FlatSpec with Matchers {
  import org.scalatest.OptionValues._

  implicit val secStore = new InMemorySecretStore(Secrets.mockSecrets)
  implicit val marshaller = new Marshaller(secStore)
  val idGenerator = new IdGenerator

  def defaultReq: HttpRequest =
    new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")
  def mockSession: Session =
    Session(idGenerator.next, defaultReq, Tokens.empty)

  behavior of "InMemoryEncryptedSessionStore"

  val encryptedStore = new InMemoryEncryptedSessionStore

  it should "store and retrieve sessions" in {
    val s = mockSession
    encryptedStore.update(s)
    encryptedStore.get(s.id).value.equals(s) shouldBe true
  }

  it should "create a CryptKey for a session" in {
    val s = mockSession
    encryptedStore.cryptKey(s.id).isSuccess shouldBe true
    encryptedStore.cryptKey(s.id.asString).isSuccess shouldBe true
  }

  it should "fail to create a CryptKey for an invalid session" in {
    val s = mockSession
    val invalidSecretId = ~s.id.secretId
    val id = SessionId(s.id.expires, s.id.entropy, invalidSecretId.toByte, s.id.signature)
    encryptedStore.cryptKey(id).isFailure shouldBe true
    encryptedStore.cryptKey(id.asString).isFailure shouldBe true
  }

  behavior of "MemcachedSessionStore"

  val memcachedStore = MemcachedSessionStore("localhost:11211", Session.lifetime)

  it should "store and retrieve sessions" in {
    val s = mockSession
    memcachedStore.update(s)
    memcachedStore.get(s.id).value.equals(s) shouldBe true
  }

}
