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

  implicit val sessionIdDecoder: DecodeRequest[SessionId] =
    DecodeRequest[SessionId](s => SessionId.from[String](s))

  implicit val tokenCodec: CodecJson[Token] =
    casecodec2(Token.apply, Token.unapply)("s", "u")

  val userReader: RequestReader[User] = (
      param("e") :: param("p")
      ).as[User]

  val sessionId: RequestReader[SessionId] =
    cookie("border_session").map(_.value).as[SessionId]

  val authHeaderReader: RequestReader[Token] =
    header("X-AUTH-TOKEN").should("start with secret")(_.startsWith("supersecret")).as[Token]
}