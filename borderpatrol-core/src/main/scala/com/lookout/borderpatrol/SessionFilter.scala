package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, RoutedRequest}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/10/14.
 */
class SessionFilter
  extends SimpleFilter[RoutedRequest, FinagleResponse] {
  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- SessionFilter ------------------------------>")
    val r = service(request)
    println("<----------------------------- SessionFilter ------------------------------")
    r
  }
}
