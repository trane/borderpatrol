package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._

class SessionSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "Session"

  it should "expire" in {
    Session(sessionid.next(), 1).expired should be(false)
    Session(sessionid.expired, 1).expired should be(true)
  }

  it should "have a decent toString method" in {
    val session = sessions.create(1)
    session.toString() should be(s"Session(${session.id}, ${session.data})")
  }

  it should "be the same object in memory when equal" in {
    val id = sessionid.next()
    val data = "session"
    Session(id, data) shouldBe Session(id, data)
    Session(id, "session1") should not be Session(id, "session2")

    // generate unique ids, but same data
    sessions.create(data) should not be sessions.create(data)
  }

}
