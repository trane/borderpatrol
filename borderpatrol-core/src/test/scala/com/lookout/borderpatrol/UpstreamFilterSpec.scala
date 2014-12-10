package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp._
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.{Future, Await}
import org.jboss.netty.handler.codec.http._
import org.scalatest.{Matchers, FlatSpec}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/12/14.
 */
class UpstreamFilterSpec extends FlatSpec with Matchers{
  def mockUpstreamService(response: String) = new Service[HttpRequest, String] {
    def apply(request: HttpRequest) = Future.value(response)
  }

  def mockRequest = new RoutedRequest(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET,"/upstream"), new Session())

  def mockUpService200  = new Service [HttpRequest, FinagleResponse]{
    def apply(request: HttpRequest) = {
      Future.value(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)))
    }
  }

  def mockUpService403  = new Service [HttpRequest, FinagleResponse]{
    def apply(request: HttpRequest) = {
      Future.value(new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.FORBIDDEN)))
    }
  }

  "An UpstreamFilter" should "route appropriately" in {
    Await.result(new UpstreamFilter(None, Some(mockUpService200)).apply(mockRequest, mockUpService200)) shouldBe a [Response]
    Await.result(new UpstreamFilter(None, Some(mockUpService403)).apply(mockRequest, mockUpService403)) shouldBe a [NeedsAuthResponse]
  }
}
