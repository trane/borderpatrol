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

package com.lookout.borderpatrol

import com.twitter.util.Future

import scala.util.{Failure, Success, Try}

/**
 * This provides the specification contracts for doing auth in the form of
 * Type Classes in [[auth.Access]] and [[auth.Identity]]
 *
 * Taking SAML 2.0 and OAuth2 as example flows, we have defined a set of contracts and abstractions on those
 * contracts to allow users of this library to implement instances of their specific authentication/authorization.
 *
 * The flow for a typical SAML/OAuth2 involves a protected resource, a client (web browser), and an Identity Provider.
 * Border Patrol can act as a translation layer for external representation of access and internal representation so
 * that services behind it do not need to implement SAML/OAuth2.
 *
 * The primary abstractions are:
 *   - `Identity` the external identity provider and types, e.g. SAML IdP
 *   - `Access` the internal access provider and types, e.g. api tokens, jwt etc
 *
 */
package object auth {

  implicit class TryOps[A](val tryA: Try[A]) extends AnyVal {
    def toFuture: Future[A] = tryA match {
      case Success(a) => Future.value[A](a)
      case Failure(e) => Future.exception(e)
    }
  }
}
