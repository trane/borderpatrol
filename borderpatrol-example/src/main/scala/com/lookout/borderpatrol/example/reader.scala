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

import com.lookout.borderpatrol.sessionx._
import com.twitter.util.Future
import io.finch.HttpRequest
import io.finch.request._

import scala.util.{Failure, Success, Try}

object reader {

  import com.lookout.borderpatrol.sessionx.SessionIdInjections._
  import model._
  import io.finch.AnyOps

  implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  implicit val sessionStore = SessionStores.InMemoryStore()

  case class InvalidSessionId(msg: String) extends Exception(s"Invalid SessionID: $msg")
  val requireSessionId: RequestReader[SessionId] =
    cookie("border_session").embedFlatMap {c =>
      SessionId.from[String](c.value) match {
        case Success(id) => id.toFuture
        case Failure(e) => Future.exception(InvalidSessionId(e.getMessage))
      }
    }

  val sessionIdOption: RequestReader[Option[SessionId]] =
    cookieOption("border_session") ~> (_.flatMap(c => SessionId.from[String](c.value).toOption))

  val user: RequestReader[User] =
    param("username") ::
    param("password").shouldNot(beShorterThan(6)) ~> User

  val upstream: RequestReader[Upstream] =
    RequestReader(r => r.path.split("/").toList match {
      case Nil => "service1"
      case h :: t => t.head
    })

  def session(id: SessionId): RequestReader[PSession] =
    flattenFuture[HttpRequest, PSession](req =>
      sessionStore.get(id) map {
        case Some(s) => s
        case None => ApiKeySession(id, req, Map())
      })

  val validSession: RequestReader[SessionId] =
    header("border_session").as[SessionId]
}
