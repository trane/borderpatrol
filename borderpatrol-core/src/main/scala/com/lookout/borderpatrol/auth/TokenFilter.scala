package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessions.SessionStore
import com.twitter.finagle.{Service, SimpleFilter}
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, HttpMethod, HttpRequest, HttpResponse}

/**
 * Created by svij on 6/12/14.
 */
class TokenFilter(sessionStore: SessionStore) extends SimpleFilter[HttpRequest, HttpResponse]{
  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    System.out.println("Inside token filter. Uri="+ request.getUri())
    //TODO: append the service token if exists
    //get the master token
    val sessionId = request.getHeader("BORDER_PATROL_SESSION_ID")
    val session = sessionStore.session(sessionId)
    session.masterToken map {
      request.addHeader("master_token", _)
    }
    var f_response = service(request)
    if (request.getUri().contains("auth") && request.getMethod() == HttpMethod.POST) {
      System.out.println("Inside token filter: extracting master token")
      // reassign the response future as we are running this function onSuccess
      f_response = f_response onSuccess { resp: HttpResponse =>
        if (resp.getStatus() == HttpResponseStatus.OK) {
          //take the master token and store
          val master_token = resp.getHeader("master_token")
          System.out.println("Inside token filter: extracted the master token="+master_token + "; session-id="+sessionId)
          session.masterToken = master_token
          resp.removeHeader("master_token")
          resp.setStatus(HttpResponseStatus.MOVED_PERMANENTLY)
          resp.setHeader("Content-Length", "0")
          resp.setHeader("Location", "/b/home")
        }
      }
    }
    f_response
  }
}
