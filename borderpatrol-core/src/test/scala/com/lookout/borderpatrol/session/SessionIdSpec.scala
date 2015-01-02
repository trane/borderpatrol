package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.util.{Duration, Time}
import org.scalactic.Equality
import org.scalatest.{Matchers, FlatSpec}

class SessionIdSpec extends FlatSpec with Matchers {

  def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
  def expiredExpiry: Time = Time.fromSeconds(42)

  def mockSecret = Current(currentExpiry)
  def mockGenerator = new SessionIdGenerator
  implicit val mockSecretStore = new InMemorySecretStore(Secrets(mockSecret, None))
  implicit val marshaller = SessionIdMarshaller(mockSecretStore)
  implicit val sessionIdEq =
    new Equality[SessionId] {
      def areEqual(a: SessionId, b: Any): Boolean =
        b match {
          case s: SessionId => (a.toSeq == s.toSeq) && (a.expires.inLongSeconds == s.expires.inLongSeconds)
          case _ => false
        }
    }

  behavior of "SessionIdGenerator"

  it should "create valid SessionId instances" in {
    val sid = mockGenerator.next
    val sig = mockSecretStore.current.sign(sid.payload)
    sid.expired shouldBe false
    sid.signature shouldEqual sig
    sid.secretId shouldEqual mockSecretStore.current.id
    sid.entropy should have size Constants.SessionId.entropySize
  }

  behavior of "SessionIdMarshaller"

  it should "create a base64 string from a SessionId" in {
    val sid = mockGenerator.next
    implicit lazy val bytes2String = Injection.connect[Array[Byte], Base64String, String]
    val str = bytes2String(sid.toBytes)
    val encoded = marshaller.encode(sid)
    encoded shouldBe a [String]
    encoded should fullyMatch regex ("[a-zA-Z0-9+/]+")
    encoded shouldEqual str
  }

  it should "create a SessionId from a valid base64 string" in {
    val sid = mockGenerator.next
    val sidPrime = marshaller.decode(marshaller.encode(sid)).get
    sidPrime shouldEqual sid
  }
}
