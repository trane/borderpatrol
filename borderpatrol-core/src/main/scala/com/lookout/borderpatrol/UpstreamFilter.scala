package com.lookout.borderpatrol

/**
 * The UpstreamFilter is responsible for calling the UpstreamsService and returning non 401 responses
 * back up the chain. For 401 responses, it calls an optional Service in line to get an Authorization
 * token for the upstream service that returned the 401. If that succeeeds, it makes a call back to
 * the service once more with the Authorization Token
 */

import com.lookout.borderpatrol.BorderPatrolApp._
import com.twitter.finagle.{Filter, SimpleFilter, Service}
import com.twitter.util.{Await}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 *
 * @param next Service to call if we get a 401 from the Upstream Service
 * @param uService Service to call instead of calling the service passed in the apply. We do this to accommodate the
 *                 need to have another service called after the Upstream Service.
 */
class UpstreamFilter(next: Option[Service[RoutedRequest, FinagleResponse]], uService: Option[Service[RoutedRequest, FinagleResponse]])
  extends SimpleFilter[RoutedRequest, FinagleResponse] {
  def apply(request:  RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- UpstreamFilter ------------------------------>")
    val responseFuture = uService match  {
      case None => service(request)
      case Some(t) => {
        val authorizedRequest = addRequiredHeaders(addAuthHeader(request, None))
        t(authorizedRequest)
      }
    }
    val response = Await.result(responseFuture)
    val r = response match {
      case NeedsAuthResponse(_) => {
        next match {
          case None => responseFuture
          case Some(s) => {
            val respFuture = s(request)
            val response = Await.result(respFuture)
            val authorizedRequest = addRequiredHeaders(addAuthHeader(request, Option(response)))
            val responseFuture = uService match  {
              case None => service(request)
              case Some(t) => {
                t(authorizedRequest)
              }
            }
          }
        }
        responseFuture
      }
      case Response(_) => {
        responseFuture
      }
    }
    println("<----------------------------- UpstreamFilter ------------------------------")
    r
  }

  /**
   * Update Session tokens with tokens returned in response
   * @param rq RoutedRequest
   * @param response  Response
   * @return New RoutedRequest
   */
  def updateSession(rq: RoutedRequest, response: FinagleResponse):RoutedRequest = {
    //TODO: Get tokens from Response
    val request = new RoutedRequest(rq.httpRequest, rq.session)
    request
  }

  /**
   * Add Authorization Header to the RoutedRequest.
   * @param rq RoutedRequest
   * @param responseOpt Optional Response
   * @return New RoutedRequest
   */
  def addAuthHeader(rq: RoutedRequest, responseOpt: Option[FinagleResponse]): RoutedRequest = {
    val request = responseOpt match {
      case None => {
        val request = rq.session.token("flexd") match {
          case None => new RoutedRequest(rq.httpRequest, rq.session)
          case Some(t) => {
            val r = new RoutedRequest(rq.httpRequest, rq.session)
            r.headers().remove("Auth-Token")
            r.headers().add("Auth-Token", t)
            r
          }
         }
        request
      }
      case Some(resp) =>{
        val request = updateSession(rq, resp)
        request.headers().remove("Auth-Token")
        val token = rq.session.token("flexd") match {
          case None => Nil
          case Some(t) => t
        }
        request.headers().add("Auth-Token", token)
        request
      }
    }
    request
  }

  /**
   * Add required headers in order to call upstream service
   * @param rq RoutedRequest
   * @return New RoutedRequest
   */
  def addRequiredHeaders(rq:RoutedRequest): RoutedRequest = {
    //TODO: Figure out a better way of setting headers
    val request = new RoutedRequest(rq.httpRequest, rq.session)
    //val request = RequestBuilder(rq).withHeaders(("Via","Borderpatrol"))

    //request.headers().clear()
    //TODO: Figure out what headers we need to pass and whether we need to clear all headers beforehand
    request.headers().remove("Via")
    request.headers().add("Via", "Borderpatrol")
    request
  }
}