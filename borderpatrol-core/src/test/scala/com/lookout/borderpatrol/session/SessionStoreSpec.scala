package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.Session
import com.lookout.borderpatrol.session.id.{Marshaller, Generator => IdGenerator}
import com.lookout.borderpatrol.session.secret.InMemorySecretStore
import com.lookout.borderpatrol.session.store.{MemcachedSessionStore, InMemoryEncryptedSessionStore}
import com.twitter.finagle.memcached
import com.twitter.io.Charsets
import com.twitter.util.{Duration, Future}
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.handler.codec.http.{HttpRequest, DefaultHttpRequest, HttpMethod, HttpVersion}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class SessionStoreSpec extends FlatSpec with Matchers with MockFactory {
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

  def toCb(s: String): ChannelBuffer =
    ChannelBuffers.copiedBuffer(s.getBytes(Charsets.Utf8))

  class MemcachedMockSessionStore(client: memcached.BaseClient[String]) extends MemcachedSessionStore(dest = "", timeout = Duration(1, TimeUnit.SECONDS)) {
    override val store = client
  }

  it should "store and retrieve sessions" in {
    val s = mockSession
    val memcachedClient = mock[memcached.BaseClient[String]]

    val flag = 0 // meaningless part of the protocol

    /* re-enable after https://github.com/paulbutcher/ScalaMock/issues/39#issuecomment-71727931
    (memcachedClient.set(_: String, _: Int, _: Time, _: String))
      .expects(s.id.asString, flag, Time.now, s.asJson)
      .returns(Future.value((): Unit))
    */

    (memcachedClient.get(_: String))
      .expects(s.id.asString)
      .returning(Future.value(Some(s.asJson)))

    val memcachedStore = new MemcachedMockSessionStore(memcachedClient)

    // memcachedStore.update(s) // re-enable after https://github.com/paulbutcher/ScalaMock/issues/39#issuecomment-71727931
    memcachedStore.get(s).value.equals(s) shouldBe true
  }

}
