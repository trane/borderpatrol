package com.lookout.borderpatrol.test.security

import com.lookout.borderpatrol.security.HeaderFilter
import com.lookout.borderpatrol.security.Headers
import com.lookout.borderpatrol.test._
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.{Response, Cookie, Request, Status}
import com.twitter.util.Future


class HeadersSpec extends BorderPatrolSuite {

  behavior of "HeaderFilter"
  val defaults = Headers.all
  val filter = HeaderFilter(defaults)

  it should "inject all of the headers" in {
    val request = Request("/")
    filter(request, testService(r => {
      defaults.toSet.subsetOf(r.headerMap.toSet)
    })).results.status should be(Status.Ok)
  }

  it should "append X-Forwarded-For to an existing list" in {
    val request = Request("/")
    request.xForwardedFor = "10.10.10.10"
    filter(request, testService(r =>
      r.xForwardedFor.getOrElse("").split(",").size > 1)
    ).results.status should be (Status.Ok)
  }

  it should "override existing headers" in {
    val request = Request("/")
    request.headerMap.add("X-Download-Options", "arglebargle")
    filter(request, testService(r =>
      r.headerMap("X-Download-Options") == Headers.XDownloadOptions._2
    )).results.status should be(Status.Ok)
  }
}
