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

package com.lookout.borderpatrol.example
import argonaut._, Argonaut._

import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.finagle.{SimpleFilter, Service}
import com.twitter.io.Buf
import com.twitter.util.Future
import io.finch.response.{ResponseBuilder, Ok}

object service {
  import model._
  import reader._
  import com.lookout.borderpatrol.sessionx._
  import io.finch.request._
  import io.finch.argonaut._
  import com.twitter.finagle.httpx.Status

  /**
   * Handles login requests, routing them to the Token Service
   * It then will send the user to the location they were trying to access before they were prompted to login
   */
  object LoginService extends Service[Request, Response] {
    import io.finch.response._

    val loginForm =
      """<html><body>
        |<h1>Account Service Login</h1>
        |<form action="/login" method="post">
        |<label>username</label><input type="text" name="username" />
        |<label>password</label><input type="password" name="password" />
        |<input type="submit" name="login" value="login" />
        |</form>
        |</body></html>
      """.stripMargin

    val ok: Future[Response] = Future.value(Ok(Buf.Utf8(loginForm)))

    def apply(req: Request): Future[Response] =
      (for {
        u <- param("username")(req)
        p <- param("password")(req)
        s <- sessionReader(req)
        tr <- TokenService(Request("e" -> u, "p" -> p, "s" -> "login"))
        if tr.status == Status.Ok
        s2 <- Session(tr.content)
      } yield buildAuthResponse(s, s2)) or Future.value(Unauthorized("invalid login"))

    def buildAuthResponse(prev: Session[Request], next: Session[Buf]): Response =
      ResponseBuilder(Status.TemporaryRedirect)
        .withHeaders("Location" -> prev.data.uri)
        .withCookies(generateCookie(next.id))()

  }

  /**
   * If the request results in an Unauthorized status, then it will create a redirect and attach a cookie/session
   * sending you to the login page
   */
  val sessionRequestFilter: SimpleFilter[Request, Response] = new SimpleFilter[Request, Response] {

    def apply(request: Request, service: Service[Request, Response]): Future[Response] =
      service(request) flatMap {rep => rep.status match {
        case Status.Unauthorized => buildLoginResponse(request)
        case _ => Future.value(rep)
      }}

    def buildLoginResponse(request: Request): Future[Response] =
      Session(request) map { session =>
        ResponseBuilder(Status.TemporaryRedirect)
            .withHeaders("Location" -> "/login")
            .withCookies(generateCookie(session.id))()
      }
  }

  /**
   * Basic ACL service
   *
   * Generates service tokens for users who have access to that service
   * based on a mapping between services and users
   */
  object TokenService extends Service[Request, Response] {
    val authMap: Map[String, User] = Map(("login" -> User("test@example.com", "test")))

    def apply(req: Request): Future[Response] =
      for {
        u <- userReader(req)
        s <- param("s")(req)
        if authMap(s) == u
      } yield Ok(Token.generate(u))
  }

  case class ExternalService(name: String, allowed: Set[User]) extends Service[Request, Response] {
    def apply(req: Request): Future[Response] =
      for {
        t <- authHeaderReader(req)
        if allowed(t.u)
      } yield Ok(s"Hello ${t.u}! Welcome to $name")
  }

  val service1 = ExternalService("service1", Set(User("test@example.com", "test"), User("tester@examp.ly", "password")))
  val service2 = ExternalService("service1", Set(User("test@example.com", "test")))

}
