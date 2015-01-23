package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import argonaut.Argonaut._
import argonaut.CodecJson
import com.lookout.borderpatrol.session.secret.{SecretStoreApi, SecretData, InMemorySecretStore}
import com.lookout.borderpatrol.session.tokens._
import com.lookout.borderpatrol.session.id._
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

  implicit class SessionJsonEncode(val s: Session) extends AnyVal {
    def asJson: String =
      SessionCodecJson.encode(s).toString
  }

  implicit class SessionJsonDecode(val s: String) extends AnyVal {
    def asSession: Option[Session] =
      s.decodeOption[Session]
  }

  //TODO: This should be configurable(should be Memory for unit tests, and consul in run mode
  //def getSecretStore: SecretStoreApi = ConsulSecretStore(ConsulSecretsWatcher(new ConsulService))
  def getSecretStore: SecretStoreApi = InMemorySecretStore(Secrets(Secret(SecretExpiry.currentExpiry), Secret(Time.fromSeconds(100))))

}

