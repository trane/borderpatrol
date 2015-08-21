/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Lookout, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.lookout.borderpatrol.sessionx

class SessionSpec extends BorderPatrolSuite {
  import helpers._

  behavior of "Session"

  it should "expire" in {
    Session(sessionid.next, 1).expired should be(false)
    Session(sessionid.expired, 1).expired should be(true)
  }

  it should "have a decent toString method" in {
    val session = sessions.create(1)
    session.toString() should be(s"Session(${session.id}, ${session.data})")
  }

  it should "be the same object in memory when equal" in {
    val id = sessionid.next
    val data = "session"
    Session(id, data) shouldBe Session(id, data)
    Session(id, "session1") should not be Session(id, "session2")

    // generate unique ids, but same data
    sessions.create(data) should not be sessions.create(data)
  }

}
