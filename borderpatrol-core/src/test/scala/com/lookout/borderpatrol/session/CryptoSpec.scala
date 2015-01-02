package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit
import com.twitter.util.{Duration, Time}
import org.scalatest.{FlatSpec, Matchers}

class CryptoSpec extends FlatSpec with Matchers {

  behavior of "Generator"

  it should "create entropy the size of input" in {
    Generator(1) should have size 1
  }

  it should "create new values on apply" in {
    val g1 = Generator(2)
    val g2 = Generator(2)
    g1 should not equal g2
  }

}
