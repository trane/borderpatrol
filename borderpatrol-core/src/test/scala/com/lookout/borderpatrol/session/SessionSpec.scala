package com.lookout.borderpatrol.session

import org.scalatest.{FlatSpec, Matchers}

class SessionSpec extends FlatSpec with Matchers {
  implicit val secretStore = InMemorySecretStore(Secrets(Current(SecretExpiry.currentExpiry), None))
  implicit val marshaller = SessionIdMarshaller(secretStore)
  implicit val generator: SessionIdGenerator = new SessionIdGenerator
  def mockSessionStore = new InMemorySessionStore

  behavior of "Session"

  it should "create and save a new session" in {
  }
}
