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

import com.lookout.borderpatrol.ServiceMatcher
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth.keymaster._
import com.lookout.borderpatrol.auth.keymaster.Tokens._
import com.lookout.borderpatrol.server.ServerConfig
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.finagle.stats.StatsReceiver
import com.twitter.io.Buf
import com.twitter.finagle.http.{Method, Request, Response, Status}
import com.twitter.finagle.http.service.RoutingService
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
        email <- request.getParam("email").toFuture
        pass <- request.getParam("password").toFuture
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

    RoutingService.byPathObject {
      case keymasterAccessManager.path => mockKeymasterAccessIssuerService
      case keymasterIdManager.path => mockKeymasterIdentityService
      case path if checkpointLoginManager.protoManager.getOwnedPaths.contains(path) => mockCheckpointService
      case _ => mockUpstreamService
    }
  }
}

object service {

  /**
   * Get IdentityProvider map of name -> Service chain
   *
   * As of now, we only support `keymaster` as an Identity Provider
   */
  def identityProviderChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
  Map[String, Service[SessionIdRequest, Response]] =
    Map("keymaster" -> keymasterIdentityProviderChain(sessionStore))

  /**
   * Get AccessIssuer map of name -> Service chain
   *
   * As of now, we only support `keymaster` as an Access Issuer
   */
  def accessIssuerChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
  Map[String, Service[SessionIdRequest, Response]] =
    Map("keymaster" -> keymasterAccessIssuerChain(sessionStore))

  /**
   * The sole entry point for all service chains
   */
  def MainServiceChain(implicit config: ServerConfig, statsReceiver: StatsReceiver): Service[Request, Response] = {
    implicit val secretStore = config.secretStore
    val serviceMatcher = ServiceMatcher(config.customerIdentifiers, config.serviceIdentifiers)

    RoutingService.byPath {
      case "/logout" =>
        ExceptionFilter() andThen /* Convert exceptions to responses */
          ServiceFilter(serviceMatcher) andThen /* Validate that its our service */
          LogoutService(config.sessionStore)
      case _ =>
        ExceptionFilter() andThen /* Convert exceptions to responses */
          ServiceFilter(serviceMatcher) andThen /* Validate that its our service */
          SessionIdFilter(config.sessionStore) andThen /* Get or allocate Session/SignedId */
          BorderService(identityProviderChainMap(config.sessionStore),
            accessIssuerChainMap(config.sessionStore)) /* Glue that connects to identity & access service */
    }
  }
}
