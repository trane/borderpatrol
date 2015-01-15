package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.{Time, Await, Duration}
import org.jboss.netty.handler.codec.http.HttpRequest

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}


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

  implicit class SessionIdAndSecretDeserialize(val s: String) extends AnyVal {
    def asSessionIdAndSecret(implicit marshaller: SessionIdMarshaller): Try[(SessionId, Secret)] =
      marshaller.decodeWithSecret(s)
  }

  implicit class SessionIdAndSecret(val s: SessionId) extends AnyVal {
    def asSessionIdAndSecret(implicit marshaller: SessionIdMarshaller): Try[(SessionId, Secret)] =
      marshaller.injector.idAndSecret2Id.invert(s)
  }

  implicit val formats = Serialization.formats(NoTypeHints)

  implicit class SecretsJsonEncode(val ss: Secrets) extends AnyVal {
    def asJson(implicit formats: Formats): String =
      write(ss)
  }

  implicit class SecretsJsonDecode(val json: String) extends AnyVal {
    def asSecrets: Secrets =
      read[Secrets](json)
  }

  trait SecureSession {
    val id: SessionId
    val originalRequest: HttpRequest
    val tokens: Tokens
  }

  case class Session(id: SessionId, originalRequest: HttpRequest, tokens: Tokens) extends SecureSession

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
    implicit val secretStore = InMemorySecretStore(Secrets(Secret(currentExpiry), Secret(Time.fromSeconds(100))))
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

}
