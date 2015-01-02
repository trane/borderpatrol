package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers}
import com.twitter.util.{Duration, Time}

class SecretSpec extends FlatSpec with Matchers {

  def currentExpiry: Time = SecretExpiry.currentExpiry

  behavior of "Secret"

  it should "expire in a day" in {
    val currentSecret = Current(currentExpiry)
    currentSecret.expiry.moreOrLessEquals(Time.now, Duration(1, TimeUnit.DAYS)) shouldBe true
  }

  "A Secret" should "be comparable" in {
    val currentSecret = Current(currentExpiry)
    currentSecret shouldEqual currentSecret
    currentSecret should not equal Current(Time.fromSeconds(0))
  }

  it should "have a relatively unique id" in {
    val expires = currentExpiry
    val sameId = Current(expires).id == Current(expires).id && Current(expires).id == Current(expires).id
    sameId shouldBe false
  }
}
