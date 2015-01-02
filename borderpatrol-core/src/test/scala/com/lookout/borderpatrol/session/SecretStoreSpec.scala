package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers}
import com.twitter.util.{Duration, Time}

class SecretStoreSpec extends FlatSpec with Matchers {

  def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
  def expiredExpiry: Time = Time.fromSeconds(42)

  behavior of "SecretStore"

  it should "never give an expired Secret with .current" in {
    val cur = Current(expiredExpiry)
    val ss = InMemorySecretStore(Secrets(cur, None))
    cur.expiry < Time.now shouldBe true
    ss.current should not equal cur
    ss.current.expiry > Time.now shouldBe true
  }

  it should "place the previously expired current into previous" in {
    val cur = Current(expiredExpiry)
    val ss = InMemorySecretStore(Secrets(cur, None))
    ss.current should not equal cur
    ss.previous.forall(p => p.key == cur.key && p.expiry == cur.expiry) shouldBe true
  }

  it should "find the proper secret based on a predicate" in {
    val cur = Current(currentExpiry)
    val prev = Previous(Current(currentExpiry))
    val ss = InMemorySecretStore(Secrets(cur, Some(prev)))
    val g = Generator(1)
    val s1 = ss.current.sign(g)
    val s2 = ss.previous.get.sign(g)
    ss.find(_.sign(g).sameElements(s1)).get shouldBe cur
    ss.find(_.sign(g).sameElements(s2)).get shouldBe prev
  }

}
