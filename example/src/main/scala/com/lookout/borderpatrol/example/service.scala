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

import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.auth.keymaster._
import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth.keymaster.Tokens._
import com.lookout.borderpatrol.{ServerConfig, ServiceMatcher}
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.io.Buf
import com.twitter.finagle.httpx.{Method, Request, Response, Status}
import com.twitter.finagle.httpx.path._
import com.twitter.finagle.httpx.service.RoutingService
import com.twitter.finagle.{Httpx, Service}
import com.twitter.util.Future
import io.finch.response.ResponseBuilder


object service {

  //  Mock Keymaster identityProvider
  val mockKeymasterIdentityService = new Service[Request, Response] {

    val userMap: Map[String, String] = Map(
      ("test1@example.com" -> "password1"),
      ("test2@example.com" -> "password2"),
      ("test3@example.com" -> "password3")
    )

    def apply(request: Request): Future[Response] = {
      val tokens = Tokens(MasterToken("masterT"), ServiceTokens())
      (for {
        email <- request.getParam("e").toFuture
        pass <- request.getParam("p").toFuture
        if userMap(email) == (pass)
      } yield tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens).toString()
          res.contentType = "application/json"})) handle {
        case ex => Response(Status.Unauthorized)
      }
    }
  }

  //  Mock Keymaster AccessIssuer
  val mockKeymasterAccessIssuerService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] = {
      val serviceName = request.getParam("services")
      val tokens = Tokens(MasterToken("masterT"), ServiceTokens().add(
        serviceName, ServiceToken(s"SomeServiceData:${serviceName}")))
      tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens).toString()
        res.contentType = "application/json"
      }).toFuture
    }
  }

  //  Mock Login Service
  val loginGetService = new Service[Request, Response] {
    val loginForm = Buf.Utf8(
      """<html><body>
        |<h1>Example Account Service Login</h1>
        |<form action="/login" method="post">
        |<label>username</label><input type="text" name="username" />
        |<label>password</label><input type="password" name="password" />
        |<input type="submit" name="login" value="login" />
        |</form>
        |</body></html>
      """.stripMargin
    )

    def apply(req: Request): Future[Response] = {
      req.method match {
        case Method.Get => {
          val rb = ResponseBuilder(Status.Ok).withContentType(Some("text/html"))
          rb(loginForm).toFuture
        }
        case _ => Future.value(Response(Status.NotFound))
      }
    }
  }

  //  Mock Upstream service
  val mockUpstreamService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] = {
      val serviceName = request.getParam("services")
      tap(Response(Status.Ok))(res => {
        res.contentString =
          s"""
          |<html><body>
          |<h1>Welcome to Service @(${request.path})</h1>
          |</body></html>
        """.stripMargin
        res.contentType = "text/html"
      }).toFuture
  }
  }

  val tempUpstreamService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] = {
      tap(Response(Status.Ok))(res => {
        res.contentString =
          s"""
             |<html><body>
             |<h1>Welcome to TEMP Service @(${request.path})</h1>
             |</body></html>
        """.stripMargin
        res.contentType = "text/html"
      }).toFuture
    }
  }

  //  Clients & paths to those mocked services
  //***FIXME to config
  val keymasterIdentityProviderPath = Path("identityProvider")
  val keymasterAccessIssuerPath = Path("accessIssuer")
  val keymasterClient = Httpx.newService("localhost:8081")
  val upstreamClient = Httpx.newService("localhost:8081")

  //  Login path
  def loginPostService(implicit config: ServerConfig): Service[Request, Response] = {
    implicit val secretStore = config.secretStore
    val serviceMatcher = ServiceMatcher(config.serviceIdentifiers)

    new ExceptionFilter andThen
    new ServiceFilter(serviceMatcher) andThen
    new SessionIdFilter(config.sessionStore) andThen
    new KeymasterLoginFilter(config.sessionStore) andThen
    new KeymasterIdentityProvider(keymasterClient, keymasterIdentityProviderPath)
  }

  //  All other requests
  def allOtherRequests(implicit config: ServerConfig): Service[Request, Response] = {
    implicit val secretStore = config.secretStore
    val serviceMatcher = ServiceMatcher(config.serviceIdentifiers)

    new ExceptionFilter andThen
    new ServiceFilter(serviceMatcher) andThen
    new SessionIdFilter(config.sessionStore) andThen
    new IdentityFilter[Tokens](config.sessionStore) andThen
    new KeymasterAccessFilter(upstreamClient) andThen
    new KeymasterAccessIssuer(keymasterClient, keymasterAccessIssuerPath, config.sessionStore)
  }

  def getRoutingService(implicit config: ServerConfig) = {
    val idpServiceIds = config.serviceIdentifiers.filter(a => a.name startsWith "ck")
    val idpPostPathMap = idpServiceIds.map(a => (a.path -> loginPostService)).toMap
    val idpGetPathMap = idpServiceIds.map(a => (a.path -> loginGetService)).toMap

    RoutingService.byMethodAndPathObject {
      case Method.Post -> path if idpPostPathMap.contains(path) => idpPostPathMap(path) //loginPostService
      case Method.Get -> path if idpGetPathMap.contains(path) => idpGetPathMap(path) //loginGetService
      case _ => allOtherRequests //FIXME: Should we restrict this to known upstream paths only
    }
  }

  def getMockRoutingService(implicit config: ServerConfig) = {
    val protectedServiceIds = config.serviceIdentifiers.filterNot(a => a.name startsWith "ck")
    val mockProtectedPathMap = protectedServiceIds.map(a => (a.path -> mockUpstreamService)).toMap
    val mockMap = mockProtectedPathMap + (keymasterAccessIssuerPath -> mockKeymasterAccessIssuerService,
      keymasterIdentityProviderPath -> mockKeymasterIdentityService)

    RoutingService.byMethodAndPathObject {
      case Method.Post -> path if mockMap.contains(path) => mockMap(path) //mockKeymasterIdentityService
      case Method.Post -> path if mockMap.contains(path) => mockMap(path) // mockKeymasterAccessIssuerService
      case _ -> path if mockMap.contains(path) => mockMap(path) // */ => mockUpstreamService
    }
  }
}
