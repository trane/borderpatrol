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

package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol._
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.util.Future

import scala.util.{Failure, Try, Success}

trait AuthImplicits extends AuthTypeClasses {

  object BasicAuth {
    object Injections {
      import com.twitter.bijection._
      import com.twitter.io.Charsets

      lazy implicit val string2Bytes: Bijection[String, Array[Byte]] =
        new AbstractBijection[String, Array[Byte]] {
          def apply(s: String): Array[Byte] =
            s.getBytes(Charsets.Utf8)
          override def invert(bytes: Array[Byte]): String =
            new String(bytes, Charsets.Utf8)
        }

      lazy implicit val credential2String: Injection[Credential, String] =
        new AbstractInjection[Credential, String] {
          def apply(c: Credential): String =
            s"${c.username}:${c.password}"
          override def invert(s: String): Try[Credential] = {
            s.split(":", 2).toList match {
              case h :: t => Success(Credential(h, t.mkString))
              case _ => Failure(new Exception(s"$s is not valid basic auth"))
            }

          }
        }

      lazy implicit val b64toBasic: Injection[Base64String, BasicAuthString] =
        new AbstractInjection[Base64String, BasicAuthString] {
          def apply(b: Base64String): BasicAuthString =
            s"Basic $b"
          override def invert(s: BasicAuthString): Try[Base64String] = {
            s.split("Basic ").toList match {
              case _ :: auth :: Nil => Success(Base64String(auth))
              case _ => Failure(new Exception(s"$s is not using basic auth"))
            }
          }
        }

      lazy val credential2Basic = Injection.connect[Credential, String, Array[Byte], Base64String, BasicAuthString]
    }

    implicit def basic2Credential: BasicAuthString => Try[Credential] =
      Injections.credential2Basic.invert(_)

    implicit object BorderAuthBasic extends BorderAuth[Basic]

    implicit object BasicAuthable extends Authable[Basic] {

      def validate(request: AuthRequest[Basic]): Future[AuthResult[Basic]] = request.auth match {
        case Some(x) => Basic(x).credential match {
          case Success(c) => AuthResult(Basic(x)).toFuture
          case _ => Future.exception(new UnauthorizedRequest())
        }
        case _ => Future.exception(new UnauthorizedRequest())
      }

      def identify(request: AuthResourceRequest[Basic]): Future[AuthInfo[Basic]] = request.auth match {
        case Some(x) => AuthInfo(Basic(x)).toFuture
        case _ => Future.exception(new UnauthorizedRequest())
      }
    }


  }

}
