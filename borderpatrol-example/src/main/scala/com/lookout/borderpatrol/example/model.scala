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

import argonaut._
import Argonaut._
import com.twitter.finagle.Service
import com.twitter.io.Charsets
import com.twitter.util.{Future, Base64StringEncoder}
import io.finch.request.PRequestReader
import io.finch.request.items.{MultipleItems, RequestItem}
import io.finch.{HttpResponse, HttpRequest}
import com.lookout.borderpatrol.sessionx._

object model {

  type Upstream = String

  case class ApiKey(upstream: Upstream, key: String) extends ToJson {
    lazy val base64: String = Base64StringEncoder.encode(s"$upstream$key".getBytes(Charsets.Utf8))
    override def toJson = Json.obj("upstream" := upstream, "key" := key)
  }

  case class User(username: String, password: String)

  case class LoginRequest(user: User, service: String, request: HttpRequest)
  case class LoginResponse(apiKey: ApiKey, response: HttpResponse)

  case class ApiKeySession(id: SessionId, request: HttpRequest, data: Map[Upstream, ApiKey]) extends HttpSession[Map[Upstream, ApiKey]]

  case class Foo(foo: String) extends ToJson {
    def toJson = Json.obj("foo" := foo)
  }
  case class Bar(bar: Int) extends ToJson {
    def toJson = Json.obj("bar" := bar)
  }

  def embed[R,A](i: RequestItem)(f: R => Future[A]): PRequestReader[R, A] =
    new PRequestReader[R, A] {
      val item = i
      def apply(req: R): Future[A] = f(req)
    }

  def flattenFuture[R, A](f: R => Future[A]): PRequestReader[R, A] =
    embed[R, A](MultipleItems)(f(_))

  trait ToJson { def toJson: Json }

  object TurnModelIntoJson extends Service[ToJson, Json] {
    def apply(model: ToJson): Future[Json] =
      Future.value[Json](model.toJson)
  }

  implicit class SeqToJson[A](s: Service[A, Seq[ToJson]]) extends Service[A, ToJson] {
    private [example] def seqToJson(seq: Seq[ToJson]) = new ToJson {
      def toJson: Json = Json.array(seq.map { _.toJson }: _*)
    }

    def apply(req: A): Future[ToJson] =
      s(req) map seqToJson
  }
}
