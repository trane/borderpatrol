package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.session.secret.InMemorySecretStore
import com.twitter.util.{Duration, Time}
import org.scalatest.{FlatSpec, Matchers}

class SecretStoreSpec extends FlatSpec with Matchers {

  def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
  def expiredExpiry: Time = Time.fromSeconds(42)

  behavior of "SecretStore"

  it should "never give an expired Secret with .current" in {
    val cur = Secret(expiredExpiry)
    val ss = InMemorySecretStore(Secrets(cur, Secret(Time.fromSeconds(0))))
    cur.expiry < Time.now shouldBe true
    ss.current should not equal cur
    ss.current.expiry > Time.now shouldBe true
  }

  it should "place the previously expired current into previous" in {
    val cur = Secret(expiredExpiry)
    val prev = Secret(Time.fromSeconds(0))
    val ss = InMemorySecretStore(Secrets(cur, prev))
    ss.current should not equal cur
    (ss.previous.key == cur.key && ss.previous.expiry == cur.expiry) shouldBe true
  }

  it should "find the proper secret based on a predicate" in {
    val cur = Secret(currentExpiry)
    val prev = Secret(currentExpiry)
    val ss = InMemorySecretStore(Secrets(cur, prev))
    val g = Generator(1)
    val s1 = ss.current.sign(g)
    val s2 = ss.previous.sign(g)
    ss.find(_.sign(g).sameElements(s1)).get shouldBe cur
    ss.find(_.sign(g).sameElements(s2)).get shouldBe prev
  }

}
