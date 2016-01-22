package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._
import com.twitter.finagle.http.Cookie

class SessionIdSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "SignedId"

  it should "create a sessionId with expiry" in {
    val id = sessionid.untagged
    val expiredId = sessionid.expired

    id.expired should not be true
    expiredId.expired shouldBe true
    Tag.authenticated(id.tag) shouldBe false
  }

  it should "convert to and from a string, and cookie" in {
    val id = sessionid.untagged

    SignedId.from[String](SignedId.as[String](id)).success.value should be(id)
    SignedId.from[Cookie](SignedId.as[Cookie](id)).success.value should be(id)
  }

  it should "create the same signature using the same secret" in {
    val id = sessionid.untagged
    val newSecret = Secret()

    id.signWith(newSecret) should be(SignedId.signWith(id, newSecret))
    id.signWith(newSecret) should not be(id.signature)
  }

  it should "not be derivable from string value if signed with secret not in the store" in {
    val idWithoutValidSecret = sessionid.untagged.copy(secret = secrets.invalid)

    SignedId.from[String](idWithoutValidSecret.asBase64).failure.exception should be(a [SignedIdError])
  }

  it should "not be derivable from string value if signatures don't match" in {
    val badId = sessionid.invalid
    SignedId.from[String](badId.asBase64).failure.exception should be(a [SignedIdError])
  }

  it should "not be derivable from an invalid string" in {
    SignedId.from[String]("hello world!").failure.exception should be(a [SignedIdError])
  }

  it should "create a tagged sessionId" in {
    val id = sessionid.authenticated
    Tag.authenticated(id.tag) shouldBe true
  }
}
