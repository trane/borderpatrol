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
import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.view.View
import com.twitter.bijection._
import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx
import com.twitter.io.Buf
import com.twitter.util.{Future, Base64StringEncoder, Time}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http.{HttpVersion, DefaultHttpRequest, HttpMethod, HttpRequest}
import scala.util.{Failure, Success, Try}

trait SessionImplicits extends SessionTypeClasses {

  implicit object TimeConversions extends Serializable[Time] {
    implicit def asLong(t: Time): Long =
      t.inMilliseconds
    implicit def asBytes(t: Time): TimeBytes =
      Injection.long2BigEndian(asLong(t))
    implicit def fromLong(l: Long): Time =
      Time.fromMilliseconds(l)
    implicit def fromBytes(bytes: TimeBytes): Try[Time] =
      Injection.long2BigEndian.invert(bytes.toArray).map(fromLong)
  }

  implicit object SecretConversions extends Serializable[Secret] {
    implicit def fromByte(byte: SecretId)(implicit store: SecretStoreApi): Option[Secret] =
      store.find(_.id == byte)
    implicit def asByte(s: Secret): SecretId =
      s.id
    implicit def asJson[A <: Secret : EncodeJson](s: A): Json =
      s.asJson
    implicit def asString[A <: Secret : EncodeJson](s: A): String =
      asJson(s).toString()
  }

  object SessionIdInjections {

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val payloadSize: Size = timeBytesSize + SessionId.entropySize + secretIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def invalid(sig1: Signature, sig2: Signature): Boolean =
      sig1 != sig2

    def validate(t: Time, sig1: Signature, sig2: Signature): Try[Unit] =
      if (SessionId.expired(t)) Failure(new SessionIdException(s"Expired $t"))
      else if (invalid(sig1, sig2)) Failure(new SessionIdException("Signature is invalid"))
      else Success(())

    def long2Time(l: Long): Time =
      Time.fromMilliseconds(l)

    def bytes2Long(bytes: IndexedSeq[Byte]): Try[Long] =
      Injection.long2BigEndian.invert(bytes.toArray)

    implicit def bytes2Time(bytes: IndexedSeq[Byte]): Try[Time] = for {
      l <- bytes2Long(bytes)
    } yield long2Time(l)

    implicit def byte2Secret(byte: Byte)(implicit store: SecretStoreApi): Try[Secret] =
      store.find(_.id == byte) match {
        case Some(s) => Success(s)
        case None => Failure(new SessionIdException(s"No secret with id=$byte"))
      }

    implicit def bytes2Tuple(bytes: IndexedSeq[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail) = pl.splitAt(timeBytesSize)
        val (ent, idList) = tail.splitAt(SessionId.entropySize)
        Success((pl, tb, ent, idList.head, sig))
      }
      case _ => Failure(new SessionIdException("Not a session string"))
    }

    implicit def seq2SessionId(bytes: IndexedSeq[Byte])(implicit store: SecretStoreApi): Try[SessionId] = for {
      (pyld, tbs, ent, id, sig) <- bytes2Tuple(bytes)
      time <- bytes2Time(tbs)
      secret <- byte2Secret(id)
      _ <- validate(time, sig, secret.sign(pyld))
    } yield new SessionId(time, ent, secret, sig)

    implicit def str2arr(s: String): Array[Byte] =
      Base64StringEncoder.decode(s)

    def arr2seq(bytes: Array[Byte]): IndexedSeq[Byte] =
      bytes.toIndexedSeq

    implicit def str2seq(s: String): IndexedSeq[Byte] =
      arr2seq(str2arr(s))

    implicit def str2SessionId(s: String)(implicit store: SecretStoreApi): Try[SessionId] =
      seq2SessionId(str2seq(s))

  }

  /**
   * A container for some type `A` with a unique identifier of `SessionId`
   */
  trait Session[+A] {
    val id: SessionId
    val data: A

    /**
     * Transform a `Session[A]` to a `Session[B]` given a function from `A => B`, keeping the same id
     *
     * @param f
     * @return
     */
    def map[B](f: A => B): Session[B] =
      Session(id, f(data))
  }

  type PSession = Session[_]

  object Session {
    import Injections.HttpxRequestCodecJson
    implicit val str2Buf: String => Buf = Buf.Utf8(_)
    implicit val buf2OStr: Buf => Option[String] = Buf.Utf8.unapply(_)
    implicit val req2Json: httpx.Request => Json = Injections.HttpxRequestCodecJson.encode(_)
    implicit val json2OReq: Json => Option[httpx.Request] = HttpxRequestCodecJson.Decoder.decodeJson(_).toOption
    implicit val json2String: Json => String = _.toString()
    implicit val json2Buf: Json => Buf = j => str2Buf(json2String(j))
    implicit val req2Buf: httpx.Request => Buf = j => json2Buf(req2Json(j))
    implicit val buf2OReq: Buf => Option[httpx.Request] = b =>
      buf2OStr(b).flatMap(s => Parse.decodeOption[httpx.Request](s))

    /**
     * helper view for SessionStore to lift `A => B` into `View[Session[A], Session[B]]`
     * useful for [[SessionStore.update]]
     */
    implicit def liftAtoB[A,B](implicit f: A => B): Session[A] %> Session[B] =
      View(_.map(f))

    /**
     * helper view to lift `B => Option[A]` into `View[Session[B], Option[Session[A]]]`
     * useful for [[SessionStore.get]]
     */
    implicit def liftBtoOptA[A,B](implicit f: B => Option[A]): Session[B] %> Option[Session[A]] =
      View(sb => sb.map(f).data.map(Session(sb.id, _)))


    /**
     * Primary method of recreating `Session[A]` from a given `SessionId` and data type `A`
     *
     * {{{
     *   case class Foo(i: Int)
     *
     *   val id = Await.result(SessionId.next)
     *   val data = Foo(42)
     *   val s = Session(id, data)
     *   val s2 = Session(id, data)
     *   s == s2 // true
     *   s === s2
     * }}}
     *
     * @param i the SessionId
     * @param d an arbitrary data value
     * @tparam A
     * @return a Session
     */
    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Any"))
    def apply[A](i: SessionId, d: A): Session[A] =
      new Session[A] {
        override val id: SessionId = i
        override val data: A = d

        @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.AsInstanceOf"))
        def membersCanEqual(o: Any): Boolean = {
          val s: Session[_] = o.asInstanceOf[Session[_]]
          s.id == id && s.data == data
        }

        @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.IsInstanceOf"))
        // scalastyle:off null
        def canEqual(o: Any): Boolean =
          o != null && o.isInstanceOf[Session[_]]

        override def equals(o: Any): Boolean =
          canEqual(o) && membersCanEqual(o)

        override def toString: String =
          s"Session($id, $data)"

        override def hashCode: Int =
          41 * (id.hashCode() + 41) + data.hashCode()
      }

    /**
     * Primary mechanism for generating new [[Session]], returning a `Future` of the `Session[A]`
     *
     * {{{
     *   val data = 1
     *   val sessionFuture = for {
     *    s <- Session(data)
     *    _ <- SessionStores.InMemoryStore.update(s)
     *   } yield s
     * }}}
     *
     * @param data value you want to store
     * @param store the secret store to fetch current secret
     * @tparam A
     * @return
     */
    def apply[A](data: A)(implicit store: SecretStoreApi): Future[Session[A]] =
      SessionId.next map (Session(_, data))


    /**
     * Set of implicit injections for `Session[A]` where `A => B` and `B => Option[A]`
     */
    object Injections {
      import scala.collection.JavaConverters._

      implicit val ByteCodecJson: CodecJson[Byte] =
        CodecJson(
          (b: Byte) => jNumber(b.toInt),
          c => for (b <- c.as[Int]) yield b.toByte
        )

      implicit val TimeCodecJson: CodecJson[Time] =
        CodecJson(
          (t: Time) => ("ms" := t.inMilliseconds) ->: jEmptyObject,
          c => for { s <- (c --\ "ms").as[Long] } yield Time.fromMilliseconds(s)
        )

      implicit val HttpRequestCodecJson: CodecJson[HttpRequest] =
        CodecJson(
          (r: HttpRequest) =>
            ("u" := r.getUri) ->:
              ("m" := r.getMethod.getName) ->:
              ("v" := r.getProtocolVersion.getText) ->:
              ("c" := r.getContent.array.toList) ->:
              ("h" := r.headers.names.asScala.map(n => Map[String, String](n -> r.headers.get(n)))) ->:
              jEmptyObject,
          c => for {
            uri <- (c --\ "u").as[String]
            meth <- (c --\ "m").as[String]
            ver <- (c --\ "v").as[String]
            heads <- (c --\ "h").as[List[Map[String, String]]]
            cont <- (c --\ "c").as[List[Byte]]
          } yield tap(new DefaultHttpRequest(HttpVersion.valueOf(ver), HttpMethod.valueOf(meth), uri))(req => {
              heads.foreach(head => head.foreach(kv => req.headers.add(kv._1, kv._2)))
              req.setContent(ChannelBuffers.copiedBuffer(cont.toArray))
            })
        )

      implicit val HttpxRequestCodecJson: CodecJson[httpx.Request] =
        CodecJson(
          (r: httpx.Request) => HttpRequestCodecJson.encode(Bijections.requestToNetty(r)),
          c => for (r <- HttpRequestCodecJson.decode(c)) yield Bijections.requestFromNetty(r)
        )

      implicit def str2SessionId(s: String)(implicit store: SecretStoreApi): Try[SessionId] =
        SessionIdInjections.str2SessionId(s)

      implicit def SessionIdCodecJson(implicit store: SecretStoreApi): CodecJson[SessionId] =
        CodecJson(
          (id: SessionId) => ("id" := SessionId.toBase64(id)) ->: jEmptyObject,
          c => (for {
            id <- (c --\ "id").as[String]
          } yield SessionId.from[String](id)).flatMap[SessionId] {
            case Success(id) => DecodeResult.ok(id)
            case Failure(e) => DecodeResult.fail(e.getMessage, c.history)
          }
        )

      implicit def HttpSessionCodecJson(implicit store: SecretStoreApi): CodecJson[Session[httpx.Request]] =
        CodecJson(
          (s: Session[httpx.Request]) => ("id" := s.id) ->: ("data" := s.data) ->: jEmptyObject,
          c => for {
            id <- (c --\ "id").as[SessionId]
            data <- (c --\ "data").as[httpx.Request]
          } yield Session(id, data)
        )

      implicit def arr2Buf: Bijection[Array[Byte], Buf] =
        new Bijection[Array[Byte], Buf] {
          def apply(bytes: Array[Byte]): Buf = Buf.ByteArray.Owned(bytes)
          override def invert(buf: Buf): Array[Byte] = Buf.ByteArray.Owned.extract(buf)
        }

      implicit def str2Buf: Bijection[String, Buf] =
        new Bijection[String, Buf] {
          def apply(a: String): Buf = Buf.Utf8(a)
          override def invert(b: Buf): String = b match { case Buf.Utf8(u) => u }
        }

      implicit def json2Str: Injection[Json, String] =
        new AbstractInjection[Json, String] {
          def apply(json: Json): String = json.toString()
          override def invert(s: String): Try[Json] = Parse.parseOption(s) match {
            case None => Failure(new DecodeSessionJsonException("Unable to convert string to Json"))
            case Some(v) => Success(v)
          }
        }
    }

  }

}
