package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._

class TagSpec extends BorderPatrolSuite {

  behavior of "Tag"

  it should "be able to construct appropriate tag objects" in {
    Tag(1.toByte) should be equals (AuthenticatedTag)
    Tag(0.toByte) should be equals (Untagged)
    Tag(5.toByte) should be equals (Untagged)
  }

  it should "have authenticated return true AuthenticatedTag only" in {
    Tag.authenticated(AuthenticatedTag) shouldBe true
    Tag.authenticated(Untagged) shouldBe false
    Tag.authenticated(Tag(5.toByte)) shouldBe false
  }
}
