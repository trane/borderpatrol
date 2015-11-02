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
import com.lookout.borderpatrol.{ServiceMatcher, Manager}
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.io.Buf
import com.twitter.finagle.httpx.{Method, Request, Response, Status}
import com.twitter.finagle.httpx.path._
import com.twitter.finagle.httpx.service.RoutingService
import com.twitter.finagle.Service
import com.twitter.util.Future
import io.finch.response.ResponseBuilder


object MockService {

  //  Mock Keymaster identityManager
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
          res.contentType = "application/json"
        })) handle {
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
  val mockCheckpointService = new Service[Request, Response] {
    val loginForm = Buf.Utf8(
      """<html><body>
        |<h1>Example Account Service Login</h1>
        |<form action="/signin" method="post">
        |<label>username</label><input type="text" name="username" />
        |<label>password</label><input type="password" name="password" />
        |<input type="submit" name="login" value="login" />
        |</form>
        |</body></html>
      """.stripMargin
    )

    def apply(req: Request): Future[Response] =
      req.method match {
        case Method.Get => {
          val rb = ResponseBuilder(Status.Ok).withContentType(Some("text/html"))
          rb(loginForm).toFuture
        }
        case _ => Future.value(Response(Status.NotFound))
      }
  }

  //  Mock Upstream service
  val mockUpstreamService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] =
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

  def getMockRoutingService(implicit config: ServerConfig): Service[Request, Response] = {
    val checkpointLoginManager = config.findLoginManager("checkpoint")
    val keymasterIdManager = config.findIdentityManager("keymaster")
    val keymasterAccessManager = config.findAccessManager("keymaster")

    val mockKeymasterPathMap = Map(keymasterAccessManager.path -> mockKeymasterAccessIssuerService,
      keymasterIdManager.path -> mockKeymasterIdentityService,
      checkpointLoginManager.path -> mockCheckpointService)

    RoutingService.byMethodAndPathObject {
      case _ -> path if mockKeymasterPathMap.contains(path) => mockKeymasterPathMap(path)
      case _ => mockUpstreamService
    }
  }
}

object service {

  /**
   * Glue that DEMUXes the chain into appropriate identityProvider or accessIssuer
   *
   * @param accessIssuerMap
   * @param identityProviderMap
   */
  case class MainGlueService(identityProviderMap: Map[String, Service[SessionIdRequest, Response]],
                             accessIssuerMap: Map[String, Service[SessionIdRequest, Response]])
  extends Service[SessionIdRequest, Response] {

    def getIdentityProviderService(identityManager: Manager): Service[SessionIdRequest, Response] =
      identityProviderMap.get(identityManager.name) match {
        case Some(s) => s
        case None => throw InvalidConfigError("Failed to find IdentityProvider Service Chain for " +
          identityManager.name)
      }

    def getAccessIssuerService(accessManager: Manager): Service[SessionIdRequest, Response] =
      accessIssuerMap.get(accessManager.name) match {
        case Some(s) => s
        case None => throw InvalidConfigError("Failed to find AccessIssuer Service Chain for " +
          accessManager.name)
      }

    def apply(req: SessionIdRequest): Future[Response] = {
      val p = Path(req.req.req.path)
      /* LoginPath that processes the login response */
      if (p.startsWith(req.req.serviceId.loginManager.loginPath))
        getIdentityProviderService(req.req.serviceId.loginManager.identityManager)(req)
      /* Path that handles the LoginManager specific routes */
      else if (p.startsWith(req.req.serviceId.loginManager.path))
        getIdentityProviderService(req.req.serviceId.loginManager.identityManager)(req)
      /* Upstream service path */
      else if (p.startsWith(req.req.serviceId.path))
        getAccessIssuerService(req.req.serviceId.loginManager.accessManager)(req)
      else tap(Response(Status.NotFound))(r => {
        r.contentString = s"${req.req.req.path}: No IdentityProvider or AccessIssuer found. " +
          s"Returning (${Status.NotFound.code})"
        r.contentType = "text/plain"
      }).toFuture
    }
  }

  /**
   * The sole entry point for all service chains
   */
  def MainServiceChain(implicit config: ServerConfig): Service[Request, Response] = {
    implicit val secretStore = config.secretStore
    val serviceMatcher = ServiceMatcher(config.serviceIdentifiers)
    val identityProviderMap = Map("keymaster" -> keymasterIdentityProviderChain(config.sessionStore))
    val accessIssuerMap = Map("keymaster" -> keymasterAccessIssuerChain(config.sessionStore))

    new ExceptionFilter andThen
      new ServiceFilter(serviceMatcher) andThen
      new SessionIdFilter(config.sessionStore) andThen
      new MainGlueService(identityProviderMap, accessIssuerMap)
  }
}
