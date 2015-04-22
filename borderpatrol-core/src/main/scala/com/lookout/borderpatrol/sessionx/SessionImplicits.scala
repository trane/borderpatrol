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
import com.lookout.borderpatrol.session.id.ExpiryComponent
import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.{View, %>}
import com.lookout.borderpatrol.session.SessionId
import com.twitter.bijection._
import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx
import com.twitter.io.Buf
import com.twitter.util.Time
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http.{HttpMethod, HttpVersion, DefaultHttpRequest, HttpRequest}

import scala.util.{Failure, Success, Try}

trait SessionImplicits extends SessionTypeClasses {


  object SessionIdInjections extends ExpiryComponent {
    import com.lookout.borderpatrol.session.id.Types._
    import com.lookout.borderpatrol.session.Constants.SessionId.entropySize

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val payloadSize: Size = timeBytesSize + entropySize + secretIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def long2Time(l: Long): Try[Time] = {
      val t = Time.fromMilliseconds(l)
      if (t > Time.now && t <= currentExpiry) Success(t)
      else Failure(new SessionIdException("Time has expired"))
    }

    def parseBytes(bytes: Vector[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail) = pl.splitAt(timeBytesSize)
        val (ent, idList) = tail.splitAt(entropySize)
        Success(pl, tb, ent, idList.head, sig)
      }
      case _ => Failure(new SessionIdException("Not a session string"))
    }

    def sessionId(tuple: BytesTuple): Try[SessionId] = {
      val (pl, tb, ent, id, sig) = tuple
      for {
        tLong <- Injection.long2BigEndian.invert(tb.toArray)
        time <- long2Time(tLong)
      } yield SessionId(time, ent, id, sig)
    }

    implicit lazy val sessionIdAndSecret2Bytes: Injection[SessionId, Array[Byte]] =
      new AbstractInjection[SessionId, Array[Byte]] {

        def apply(id: SessionId): Array[Byte] =
          id.toBytes.toArray

        override def invert(bytes: Array[Byte]): Try[SessionId] =
          for (t <- parseBytes(bytes.toVector); s <- sessionId(t)) yield s
      }

    implicit lazy val id2String = Injection.connect[SessionId, Array[Byte], Base64String, String]
  }

  object SessionInjections {
    import scala.collection.JavaConversions._

    implicit def ByteCodecJson: CodecJson[Byte] =
      CodecJson(
        (b: Byte) => jNumberOrNull(b.toInt),
        c => for (b <- c.as[Int]) yield b.toByte
      )

    implicit def TimeCodecJson: CodecJson[Time] =
      CodecJson(
        (t: Time) =>
          ("ms" := t.inMilliseconds) ->: jEmptyObject,
        c => for {
          s <- (c --\ "ms").as[Long]
        } yield Time.fromMilliseconds(s))

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

    implicit lazy val session2Json: Injection[HttpSessionJson, Json] = new AbstractInjection[HttpSessionJson, Json] {
      def apply(session: HttpSessionJson): Json =
        implicitly[EncodeJson[HttpSessionJson]].encode(session)
      override def invert(json: Json): Try[HttpSessionJson] =
        implicitly[DecodeJson[HttpSessionJson]].decodeJson(json).fold[Try[HttpSessionJson]](
          (str, _) => Failure(new DecodeSessionJsonException(str)),
          (session) => Success(session)
        )
    }

    implicit lazy val arr2Buf: Bijection[Array[Byte], Buf] =
      new Bijection[Array[Byte], Buf] {
        def apply(bytes: Array[Byte]): Buf = Buf.ByteArray.Owned(bytes)
        override def invert(buf: Buf): Array[Byte] = Buf.ByteArray.Owned.extract(buf)
      }

    implicit lazy val str2Buf: Bijection[String, Buf] =
      new Bijection[String, Buf] {
        def apply(a: String): Buf = Buf.Utf8(a)
        override def invert(b: Buf): String = b match { case Buf.Utf8(u) => u }
      }

    implicit lazy val session2Buf = Injection.connect[HttpSessionJson, Json, String, Array[Byte], Buf]
  }

  implicit val id2String: SessionId %> String = View(SessionIdInjections.id2String(_))
  implicit val session2Buf: HttpSessionJson %> Buf = View(SessionInjections.session2Buf(_))
  implicit val buf2Session: Buf %> Option[HttpSessionJson] = View(SessionInjections.session2Buf.invert(_).toOption)
}
