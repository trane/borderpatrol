package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import argonaut.Argonaut._
import argonaut.CodecJson
import com.lookout.borderpatrol.session.secret._
import com.lookout.borderpatrol.session.tokens._
import com.lookout.borderpatrol.session.id._
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.io.Buf
import com.twitter.util.{Time, Duration}
import org.jboss.netty.buffer.ChannelBuffers
import com.lookout.borderpatrol.util.Combinators.tap
import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpMethod, HttpVersion, HttpRequest}

import scala.collection.JavaConversions._

package object session {

  object Constants {

    object SessionId {
      val entropySize = 16
      val lifetime = Duration(1, TimeUnit.DAYS)
    }

    object Secret {
      val entropySize = 16
      val lifetime = Duration(1, TimeUnit.DAYS)
    }

  }

  lazy val bytes264 = Injection.connect[Array[Byte], Base64String, String]
  lazy val json2bytes = Injection.connect[String, Array[Byte]]

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

  implicit def SessionCodecJson: CodecJson[Session] =
    casecodec3(Session.apply, Session.unapply)("id", "req", "tokens")

  implicit class SessionOps(val s: Session) extends AnyVal {
    def asJson: String =
      SessionCodecJson.encode(s).toString

    def asBytes: Array[Byte] =
      json2bytes(s.asJson)
  }

  implicit class ArrayOps(val a: Array[Byte]) extends AnyVal {
    def asSession: Option[Session] =
      json2bytes.invert(a).toOption flatMap (_.asSession)

    def asBase64: String =
      bytes264(a)

    def asBuf: Buf =
      Buf.ByteArray.Owned(a)
  }

  implicit class BufOps(val buf: Buf) extends AnyVal {
    def asArray: Array[Byte] =
      Buf.ByteArray.Owned.extract(buf)
  }

  implicit class IndexedSeqOps(val seq: IndexedSeq[Byte]) extends AnyVal {
    def asBase64: String =
      bytes264(seq.toArray)
  }

  implicit class StringOpsSession(val s: String) extends AnyVal {
    def asSession: Option[Session] =
      s.decodeOption[Session]
  }

}

