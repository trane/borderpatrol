package com.lookout.borderpatrol.session

import com.lookout.borderpatrol.Session
import com.lookout.borderpatrol.session.id.{Marshaller, Generator => IdGenerator}
import com.lookout.borderpatrol.session.secret.InMemorySecretStore
import com.lookout.borderpatrol.session.store.InMemorySessionStore
import org.jboss.netty.handler.codec.http.{HttpMethod, HttpVersion, DefaultHttpRequest}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

class SessionSpec extends FlatSpec with Matchers {
  implicit val secretStore = InMemorySecretStore(Secrets(Secret(SecretExpiry.currentExpiry), Secret(SecretExpiry.currentExpiry)))
  implicit val marshaller = Marshaller(secretStore)
  implicit val generator = new IdGenerator
  def mockSessionStore = new InMemorySessionStore

  behavior of "Session"

  it should "be serializable" in {
    val orig = Session("servicename", new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/"))
    val json = orig.asJson
    val sOption = json.asSession
    val deser = sOption.value

    deser.id shouldEqual(orig.id)
    deser.tokens shouldEqual(orig.tokens)
    deser.originalRequest should not equal(orig.originalRequest) // mutable values
    deser.equals(orig) shouldBe true // overridden
  }
}
