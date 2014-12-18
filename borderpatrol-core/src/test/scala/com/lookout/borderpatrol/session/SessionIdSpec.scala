package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.session
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.util.{Duration, Time}
import org.scalactic.Equality
import org.scalamock.scalatest.MockFactory
import org.scalamock.proxy.ProxyMockFactory
import org.scalatest.{WordSpec, Matchers, FlatSpec}

class SessionIdSpec extends FlatSpec with Matchers with MockFactory {

  def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
  def expiredExpiry: Time = Time.fromSeconds(42)

  def mockSecret = Current(currentExpiry)
  implicit val mockSecretStore = new InMemorySecretStore(Secrets(mockSecret, None))
  implicit val marshaller = SessionIdMarshaller(mockSecretStore)
  implicit val sessionIdEq =
    new Equality[SessionId] {
      def areEqual(a: SessionId, b: Any): Boolean =
        b match {
          case s: SessionId => a.toBytes sameElements s.toBytes
          case _ => false
        }
    }

  "A SessionIdGenerator" should "create valid SessionId instances" in {
    val sgen = new SessionIdGenerator
    val sid = sgen.next
    val sig = mockSecretStore.current.sign(sid.timeBytes ++ sid.entropy :+ sid.secretId)
    sid.expired shouldBe false
    sid.signature shouldEqual sig
    sid.secretId shouldEqual mockSecretStore.current.id
    sid.entropy should have size Constants.SessionId.entropySize
  }

  "A SessionIdMarshaller" should "create a base64 string from a SessionId" in {
    val sid = (new SessionIdGenerator).next
    marshaller.encode(sid) shouldBe a [String]
    marshaller.encode(sid) should fullyMatch regex ("[a-zA-Z0-9+/]+")
  }

  it should "create a SessionId from a valid base64 string" in {
    val sid = (new SessionIdGenerator).next
    implicit lazy val bytes2String = Injection.connect[Array[Byte], Base64String, String]
    val str = bytes2String(sid.toBytes)
    marshaller.decode(marshaller.encode(sid)).get shouldEqual sid
  }
}
