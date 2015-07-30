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
import com.lookout.borderpatrol.{View, %>}
import com.lookout.borderpatrol.util.Combinators.tap
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
    implicit def fromBytes(bytes: TimeBytes): Time =
      fromLong(Injection.long2BigEndian.invert(bytes.toArray).get)
  }

  implicit object SecretConversions extends Serializable[Secret] {
    implicit def fromByte(byte: SecretId)(implicit store: SecretStoreApi): Secret =
      store.find(_.id == byte).get
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

  object CryptKey {
    import Crypto.{CryptKey => CryptKeyy}

    implicit def id2Key(id: SessionId): Array[Byte] =
      id.entropy.toArray
    implicit def sec2Iv(secret: Secret): Array[Byte] =
      secret.entropy.toArray

    def apply(id: SessionId, secret: Secret): CryptKeyy =
      new CryptKeyy(id2Key(id), sec2Iv(secret))

    def apply(id: SessionId): CryptKeyy =
      CryptKeyy(id2Key(id), sec2Iv(id.secret))

    def apply(s: PSession): CryptKeyy =
      CryptKey(s.id, s.id.secret)
  }

  object Session {
    import Injections.HttpxRequestCodecJson
    implicit val str2Buf: String %> Buf = View(s => Buf.Utf8(s))
    implicit val buf2OStr: Buf %> Option[String] = View(b => Buf.Utf8.unapply(b))
    implicit val req2Json: httpx.Request %> Json = View(r => Injections.HttpxRequestCodecJson.encode(r))
    implicit val json2OReq: Json %> Option[httpx.Request] = View(j => HttpxRequestCodecJson.Decoder.decodeJson(j).toOption)
    implicit val json2String: Json %> String = View(j => j.toString())
    implicit val json2Buf: Json %> Buf = View(j => str2Buf(json2String(j)))
    implicit val req2Buf: httpx.Request %> Buf = View(r => json2Buf(req2Json(r)))
    implicit val buf2OReq: Buf %> Option[httpx.Request] = View(b => buf2OStr(b).flatMap(s => Parse.decodeOption[httpx.Request](s)))

    implicit def s2s[A,B](implicit f: A %> B): Session[A] %> Session[B] = View(sa =>
      Session(sa.id, f(sa.data))
    )
    implicit def s2os[A,B](implicit f: B %> Option[A]): Session[B] %> Option[Session[A]] = View(sb =>
      f(sb.data).map(a => Session(sb.id, a))
    )

    def apply[A](i: SessionId, d: A): Session[A] =
      new Session[A] {
        override val id = i
        override val data = d
        override def equals(o: Any): Boolean = o match {
          case s: Session[_] => s.id == id && s.data == data
          case _ => false
        }
      }

    def apply[A](data: A)(implicit store: SecretStoreApi): Future[Session[A]] =
      SessionId.next map (Session(_, data))

    def from[A](a: A)(implicit f: A => Try[PSession]): Try[PSession] =
      f(a)

    object Injections {
      import scala.collection.JavaConversions._

      implicit def ByteCodecJson: CodecJson[Byte] =
        CodecJson(
          (b: Byte) => jNumber(b.toInt),
          c => for (b <- c.as[Int]) yield b.toByte
        )

      implicit def TimeCodecJson: CodecJson[Time] =
        CodecJson(
          (t: Time) => ("ms" := t.inMilliseconds) ->: jEmptyObject,
          c => for { s <- (c --\ "ms").as[Long] } yield Time.fromMilliseconds(s)
        )

      implicit def HttpRequestCodecJson: CodecJson[HttpRequest] =
        CodecJson(
          (r: HttpRequest) =>
            ("u" := r.getUri) ->:
              ("m" := r.getMethod.getName) ->:
              ("v" := r.getProtocolVersion.getText) ->:
              ("c" := r.getContent.array.toList) ->:
              ("h" := r.headers.names.toList.map(n => Map[String, String](n -> r.headers.get(n)))) ->:
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

      implicit def HttpxRequestCodecJson: CodecJson[httpx.Request] =
        CodecJson(
          (r: httpx.Request) => HttpRequestCodecJson.encode(Bijections.requestToNetty(r)),
          c => for (r <- HttpRequestCodecJson.decode(c)) yield Bijections.requestFromNetty(r)
        )

      implicit def str2SessionId(s: String)(implicit store: SecretStoreApi): Try[SessionId] =
        SessionIdInjections.str2SessionId(s)

      implicit def SessionIdCodecJson(implicit store: SecretStoreApi): CodecJson[SessionId] =
        CodecJson(
          (id: SessionId) => ("id" := SessionId.toBase64(id)) ->: jEmptyObject,
          c => for ( id <- (c --\ "id").as[String] ) yield SessionId.from[String](id).toOption.get
        )

      implicit def HttpSessionCodecJson(implicit store: SecretStoreApi): CodecJson[RequestSession] =
        CodecJson(
          (s: RequestSession) => ("id" := s.id) ->: ("data" := s.data) ->: jEmptyObject,
          c => for {
            id <- (c --\ "id").as[SessionId]
            data <- (c --\ "data").as[httpx.Request]
          } yield RequestSession(id, data)
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
