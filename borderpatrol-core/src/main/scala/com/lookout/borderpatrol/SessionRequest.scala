package com.lookout.borderpatrol

import com.twitter.finagle.{Filter, Service, SimpleFilter}
import com.twitter.finagle.http.Request
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}

class SessionId {

}


////  req.cookie.get {
///
case class Session(id: SessionId) {

}

case class SessionRequest(req: HttpRequest) {

}

class SessionService extends Service[HttpRequest, SessionRequest] {
  def apply(req: HttpRequest): Future[SessionRequest] = {
    ???
  }
}

class SessionFilter(sessionService: SessionService) extends Filter[HttpRequest, HttpResponse, SessionRequest, HttpResponse] {

  def apply(req: HttpRequest, service: Service[SessionRequest, HttpResponse]): Future[HttpResponse] =
    sessionService(req) flatMap { s => service(s) }
}

