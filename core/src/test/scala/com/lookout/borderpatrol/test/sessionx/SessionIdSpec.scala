package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._
import com.twitter.finagle.httpx.Cookie

class SessionIdSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "SessionId"

  it should "create a sessionId with expiry" in {
    val id = sessionid.next
    val expiredId = sessionid.expired

    id.expired should not be true
    expiredId.expired shouldBe true
  }

  it should "convert to and from a string, and cookie" in {
    val id = sessionid.next

    SessionId.from[String](SessionId.as[String](id)).success.value should be(id)
    SessionId.from[Cookie](SessionId.as[Cookie](id)).success.value should be(id)
  }

  it should "create the same signature using the same secret" in {
    val id = sessionid.next
    val newSecret = Secret()

    id.signWith(newSecret) should be(SessionId.signWith(id, newSecret))
    id.signWith(newSecret) should not be(id.signature)
  }

  it should "not be derivable from string value if signed with secret not in the store" in {
    val idWithoutValidSecret = sessionid.next.copy(secret = secrets.invalid)

    SessionId.from[String](idWithoutValidSecret.asBase64).failure.exception should be(a [SessionIdError])
  }

  it should "not be derivable from string value if signatures don't match" in {
    val badId = sessionid.invalid
    SessionId.from[String](badId.asBase64).failure.exception should be(a [SessionIdError])
  }

  it should "not be derivable from an invalid string" in {
    SessionId.from[String]("hello world!").failure.exception should be(a [SessionIdError])
  }

}
