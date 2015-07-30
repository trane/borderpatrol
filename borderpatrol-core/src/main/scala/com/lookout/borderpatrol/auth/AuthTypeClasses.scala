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

import com.lookout.borderpatrol.{BorderRequest, BorderError, AuthResourceRequest, AuthRequest}

import com.twitter.util.Future
import com.twitter.finagle.{Service, Filter, httpx}

trait AuthTypeClasses extends AuthTypes {

  trait Authable[A] {
    def validate(request: AuthRequest[A]): Future[AuthResult[A]]
    def identify(request: AuthResourceRequest[A]): Future[AuthInfo[A]]
  }

  trait AuthErrorHandler {
    def handle(e: BorderError): httpx.Response
  }

  trait BorderAuth[_] {
    def authenticate[A : Authable](request: httpx.Request): Future[AuthResult[A]] =
      implicitly[Authable[A]].validate(AuthRequest[A](request))

    def identify[A : Authable](request: httpx.Request): Future[AuthInfo[A]] =
      implicitly[Authable[A]].identify(AuthResourceRequest[A](request))
  }

  trait BorderFilter[A]
      extends Filter[httpx.Request, httpx.Response, BorderRequest[A], httpx.Response]
      with BorderAuth[A] with AuthErrorHandler {

    def apply(request: httpx.Request, service: Service[BorderRequest[A], httpx.Response]): Future[httpx.Response]

    def handle(e: BorderError): httpx.Response =
      httpx.Response(e.status)
  }
}
