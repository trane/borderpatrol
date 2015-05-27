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
import com.lookout.borderpatrol.BorderError
import com.twitter.finagle.{Service, Filter}
import com.twitter.util.Future
import io.finch.{HttpResponse, HttpRequest}
import io.finch._

import scala.collection.mutable

object service {
  import model._
  import reader._

  object ApiKeyService extends Service[HttpRequest, ApiKey] {
    val services = Set[String]("service1", "service2", "service3")

    object UserDB {
      val userDB = Set[User](User("user1", "pass1"), User("user2", "pass2"))

      def exists(u: User): Boolean =
        userDB.contains(u)
    }

    def apply(req: HttpRequest): Future[ApiKey] = for {
      u <- user(req)
      if UserDB.exists(u)
      us <- upstream(req)
      k <- (services find (_ == us)) getOrElse "defaultservice"
    } yield ApiKey(us, s"$us:$u:$k:supersecretencoded")
  }

  object Service1 extends Service[AuthRequest, Foo] {
    val name = "service1"
    val validKey = "supersecretencoded"
    def apply(req: AuthRequest): Future[Foo] = {
      req.key.base64.split(":").toList match {
        case _ :: u :: n :: k if n == name && k == validKey => Foo(s"hello $u success!42").toFuture
        case  _ =>  Foo("").toFuture
      }
    }
  }

  object Service2 extends Service[AuthRequest, Bar] {
    val name = "service2"
    val validKey = "supersecretencoded"
    def apply(req: AuthRequest): Future[Bar] = {
      req.key.base64.split(":").toList match {
        case _ :: _ :: n :: k if n == name && k == validKey => Bar(42).toFuture
        case  _ =>  Bar(1).toFuture
      }
    }
  }
}
