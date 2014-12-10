package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, RoutedRequest}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}
/**
 * Created by wkimeria on 12/11/14.
 */
class LoginFilter
  extends SimpleFilter[RoutedRequest, FinagleResponse] {
  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- LoginFilter------------------------------>")
    val r = service(request)
    println("<----------------------------- LoginFilter------------------------------")
    r
  }
}
