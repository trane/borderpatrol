package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, RoutedRequest}
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/11/14.
 */

trait AuthFilter extends Filter[RoutedRequest, FinagleResponse, RoutedRequest, FinagleResponse]

class AuthFilterSimple extends AuthFilter {
  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- AuthFilterSimple ------------------------------>")
    val r = service(request)
    println("<----------------------------- AuthFilterSimple ------------------------------")
    r
  }
}

class AuthFilterCond(uService: Service[RoutedRequest, FinagleResponse]) extends AuthFilter {
  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- AuthFilterCond ------------------------------>")
    val r = uService(request)
    println("<----------------------------- AuthFilterCond ------------------------------")
    r
  }
}