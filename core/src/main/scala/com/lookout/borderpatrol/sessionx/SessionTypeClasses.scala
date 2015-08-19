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

trait SessionTypeClasses extends SessionTypes {


  trait Encryptable[A, Key, E] {
    def apply(a: A)(key: Key): E
  }

  trait Decryptable[E, Key, A] {
    def apply(e: E)(key: Key): Option[A]
  }

  trait Crypto[A, Key, E] {
    def encrypt(a: A)(key: Key)(implicit enc: Encryptable[A, Key, E]): E =
      enc(a)(key)
    def decrypt(e: E)(key: Key)(implicit dec: Decryptable[E, Key, A]): Option[A] =
      dec(e)(key)
  }

  /*
  TODO add when crypto is done
  type SessionCrypto[E] = Crypto[PSession, SessionId, E]
  trait EncryptedStore[K, V, Key, EV, M] extends Store[K, V, M] with crypto[V, Key, EV]
  trait EncryptedSessionStore[EV, M] extends SessionStore[M] with SessionCrypto[EV]
  */
}
