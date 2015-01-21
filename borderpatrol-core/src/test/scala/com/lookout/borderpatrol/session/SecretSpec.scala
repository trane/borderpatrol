package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.twitter.util.{Duration, Time}
import org.scalatest.{FlatSpec, Matchers}

class SecretSpec extends FlatSpec with Matchers {

  def currentExpiry: Time = SecretExpiry.currentExpiry

  behavior of "Secret"

  it should "expire in a day" in {
    val currentSecret = Secret(currentExpiry)
    currentSecret.expiry.moreOrLessEquals(Time.now, Duration(1, TimeUnit.DAYS)) shouldBe true
  }

  "A Secret" should "be comparable" in {
    val currentSecret = Secret(currentExpiry)
    currentSecret shouldEqual currentSecret
    currentSecret should not equal Secret(Time.fromSeconds(0))
  }

  it should "have a relatively unique id" in {
    val expires = currentExpiry
    val sameId = Secret(expires).id == Secret(expires).id && Secret(expires).id == Secret(expires).id
    sameId shouldBe false
  }
}
