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

package com.lookout.borderpatrol.sessionx

import argonaut._
import Argonaut._
import com.lookout.borderpatrol.%>
import com.lookout.borderpatrol.session.SessionId
import com.twitter.util.Future
import com.twitter.finagle.httpx

trait SessionTypeClasses extends SessionTypes {

  trait Session[+R, +A] {
    val id: SessionId
    val request: R
    val data: A
  }
  type PSession = Session[_,_] // polymorphic session
  type HttpSession[+A] = Session[httpx.Request, A]

  object Session {
    import com.lookout.borderpatrol.auth._

    type HttpBasicSession = HttpSession[AuthInfo[Basic]]
    type HttpOAuth2Session = HttpSession[AuthInfo[OAuth2]]
  }

  trait Store[K, V, MK, MV, M] {
    val store: M

    def update(key: K)(value: V)(implicit a2k: K %> MK, b2v: V %> MV): Future[Unit]
    def get(key: K)(implicit a2k: K %> MK, v2b: MV %> Option[V]): Future[Option[V]]
  }

  trait SessionStore[K, V, M] extends Store[SessionId, PSession, K, V, M] {
    def update(key: SessionId)(value: PSession)(implicit a2k: SessionId %> K, b2v: PSession %> V): Future[Unit]
    def get(key: SessionId)(implicit a2k: SessionId %> K, v2b: V %> Option[PSession]): Future[Option[PSession]]
  }

  trait Encryptable[A] {
    def apply[E](a: A): E
  }

  trait Decryptable[E] {
    def apply[A](e: E): Option[A]
  }

  trait Crypto[A, E] {
    def encrypt(a: A)(implicit enc: Encryptable[A]): E =
      enc(a)
    def decrypt(e: E)(implicit dec: Decryptable[E]): Option[A] =
      dec(e)
  }

  object Crypto {
    def encrypt[A : Encryptable,E](a: A): E =
      implicitly[Encryptable[A]].apply(a)
    def decrypt[A, E: Decryptable](e: E): Option[A] =
      implicitly[Decryptable[E]].apply(e)
  }

  type SessionCrypto[E] = Crypto[PSession, E]

  trait EncryptedStore[K, V, MK, MV, M] extends Store[K, V, MK, MV, M] with Crypto[V, MV]
  trait EncryptedSessionStore[MK, MV, M] extends SessionStore[MK, MV, M] with SessionCrypto[MV]
}
