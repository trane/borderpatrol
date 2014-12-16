package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import com.twitter.util.Duration
import org.scalatest.mock.EasyMockSugar.MockObjects
import org.scalatest.{Matchers, FlatSpec}
import org.scalamock.scalatest.MockFactory
import org.scalamock.annotation.mockObject
import org.scalamock.annotation.mock

class CryptoSpec extends FlatSpec with Matchers with MockFactory with GeneratedMockFactory {

  "A Generator" should "create entropy the size of input" in {
    Generator(1).size shouldBe 1
  }

  it should "create new values on apply" in {
    val g1 = Generator(2)
    val g2 = Generator(2)
    g1 should not equal g2
  }

  "A SecretExpiry" should "last a day" in {
    SecretExpiry.lifetime shouldBe Duration(1, TimeUnit.DAYS)
  }

  "A Secret" should "be comparable" in {
    val currentSecret = Current(SecretExpiry.currentExpiry)
    currentSecret shouldEqual currentSecret
    currentSecret should not equal SecretStore.current
  }

  it should "have a relatively unique id" in {
    val expires = SecretExpiry.currentExpiry
    val sameId = Current(expires).id.sameElements(Current(expires).id) && Current(expires).id.sameElements(Current(expires).id)
    sameId shouldBe false
  }

  "A SecretStore" should "never give an expired Secret with .current" in {
    val ss = mock[SecretStore]

  }


}
