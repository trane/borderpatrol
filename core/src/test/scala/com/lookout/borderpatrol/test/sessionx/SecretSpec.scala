package com.lookout.borderpatrol.sessionx

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.test._
import com.twitter.util.{Time, Duration}

class SecretSpec extends BorderPatrolSuite {
  import sessionx.helpers.secrets._

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

  it should "give the current and previous Secret" in {
    secrets.current shouldEqual current
    secrets.previous shouldEqual previous
  }

}
