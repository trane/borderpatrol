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

import com.lookout.borderpatrol.session.SessionId
import com.twitter.io.Buf
import com.twitter.util.Future
import com.twitter.finagle.memcachedx

import scala.collection.mutable

package object sessionx extends SessionFunctions {

  object Stores {

    case class MemcachedStore(store: memcachedx.BaseClient[Buf])
        extends SessionStore[String, Buf, memcachedx.BaseClient[Buf]] {
      val flag = 0 // ignored flag required by memcached api

      def update(key: SessionId)(value: PSession)(implicit id2k: SessionId %> String, s2v: PSession %> Buf): Future[Unit] =
        store.set(id2k(key), flag, key.expires, s2v(value))

      def get(key: SessionId)(implicit id2k: SessionId %> String, v2s: Buf %> Option[PSession]): Future[Option[PSession]] =
        for {
          maybeBuf <- store.get(id2k(key))
          buf <- maybeBuf
        } yield v2s(buf)

    }

    case class InMemoryStore(store: mutable.Map[SessionId, PSession] = mutable.Map[SessionId, PSession]())
        extends SessionStore[SessionId, PSession, mutable.Map[SessionId, PSession]] {

      def update(key: SessionId)(value: PSession)(implicit id2k: SessionId %> SessionId, s2v: PSession %> PSession) =
        store.put(key, value).
            fold(Future.exception[Unit](new
                UpdateStoreException(s"Unable to update store with $value")))(_ => Future.value[Unit](()))

      def get(key: SessionId)(implicit id2k: SessionId %> SessionId, v2s: PSession %> Option[PSession]) =
        store.get(key).filterNot(session => session.id.expired).toFuture

    }

    case class EncryptedInMemorySessionStore(store: mutable.Map[String, Array[Byte]] = mutable.Map[String, Array[Byte]]())
        extends EncryptedSessionStore[String, Array[Byte], mutable.Map[String, Array[Byte]]] {

      def update(key: SessionId)(value: PSession)(implicit id2k: SessionId %> String, s2v: PSession %> Array[Byte]) =
        store.put(id2k(key), encrypt(value)).
            fold(Future.exception[Unit](new
                UpdateStoreException(s"Unable to update store with $value")))(_ => Future.value[Unit](()))

      def get(key: SessionId)(implicit id2k: SessionId %> String, v2s: Array[Byte] %> Option[PSession]) =
        store.get(id2k(key)).flatMap(decrypt(_)).filterNot(session => session.id.expired).toFuture
    }
  }

}
