package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import argonaut.Argonaut._
import argonaut.CodecJson
import com.twitter.util.{Time, Duration}
import org.jboss.netty.buffer.ChannelBuffers
import com.lookout.borderpatrol.util.Combinators.tap
import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpMethod, HttpVersion, HttpRequest}

import scala.collection.JavaConversions._

import scala.util.Try

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

  implicit class SessionIdSerialize(val s: SessionId) extends AnyVal {
    def asString(implicit marshaller: SessionIdMarshaller): String =
      marshaller.encode(s)
  }

  implicit class SessionIdDeserialize(val s: String) extends AnyVal {
    def asSessionId(implicit marshaller: SessionIdMarshaller): Try[SessionId] =
      marshaller.decode(s)
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

  implicit def SessionIdCodecJson: CodecJson[SessionId] =
    casecodec4(SessionId.apply, SessionId.unapply)("expires", "entropy", "secretId", "signature")

  implicit def SecretCodecJson: CodecJson[Secret] =
    casecodec3(Secret.apply, Secret.unapply)("expiry", "id", "entropy")

  implicit def SecretsCodecJson: CodecJson[Secrets] =
    casecodec2(Secrets.apply, Secrets.unapply)("current", "previous")

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

  import TokenJson.TokensCodecJson

  implicit def SessionCodecJson: CodecJson[Session] =
    casecodec3(Session.apply, Session.unapply)("id", "req", "tokens")

  implicit class SecretsJsonEncode(val ss: Secrets) extends AnyVal {
    def asJson: String =
      SecretsCodecJson.encode(ss).toString
  }

  implicit class SecretsJsonDecode(val s: String) extends AnyVal {
    def asSecrets: Option[Secrets] =
      s.decodeOption[Secrets]
  }

  implicit class SessionJsonEncode(val s: Session) extends AnyVal {
    def asJson: String =
      SessionCodecJson.encode(s).toString
  }

  implicit class SessionJsonDecode(val s: String) extends AnyVal {
    def asSession: Option[Session] =
      s.decodeOption[Session]
  }

  trait SecureSession {
    val id: SessionId
    val originalRequest: HttpRequest
    val tokens: Tokens

  }

  case class Session(id: SessionId, originalRequest: HttpRequest, tokens: Tokens) extends SecureSession {
    def equals(o: SecureSession): Boolean =
      id == o.id && tokens == o.tokens && (originalRequest.getUri == o.originalRequest.getUri &&
                                           originalRequest.getMethod == o.originalRequest.getMethod)
  }

  trait SessionFactory {
    def apply(s: String, originalRequest: HttpRequest): Session
    def apply(request: RoutedRequest): Session
  }

  object Session extends SessionFactory with SecureSessionIdComponent
                                        with SessionIdExpiryComp
                                        with SecretStoreComponent
                                        with SessionStoreComponent {

    val cookieName = "border_session"
    val entropySize = Constants.SessionId.entropySize
    implicit val secretStore = getSecretStore
    implicit val marshaller = SessionIdMarshaller(secretStore)
    implicit val generator: SessionIdGenerator = new SessionIdGenerator
    val sessionStore = new InMemorySessionStore

    def apply(s: String, originalRequest: HttpRequest): Session =
      sessionStore.get(s) getOrElse newSession(originalRequest)

    def apply(request: RoutedRequest): Session =
      request.borderCookie.flatMap(id => sessionStore.get(id)) getOrElse newSession(request.httpRequest)

    def newSession(originalRequest: HttpRequest): Session =
      Session(generator.next, originalRequest, Tokens.empty)

    def save(session: Session): Session =
      sessionStore.update(session)
  }

  //TODO: This should be configurable(should be Memory for unit tests, and consul in run mode
  //def getSecretStore: SecretStoreApi = ConsulSecretStore(ConsulSecretsWatcher)
  def getSecretStore: SecretStoreApi = InMemorySecretStore(Secrets(Secret(SecretExpiry.currentExpiry), Secret(Time.fromSeconds(100))))

}
