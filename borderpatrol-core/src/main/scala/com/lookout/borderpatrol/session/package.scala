package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import com.twitter.util.Duration
import org.jboss.netty.handler.codec.http.HttpRequest

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

  sealed trait Session {
    val id: SessionId
    val originalRequest: HttpRequest
    val tokens: Tokens
  }

  case class NewSession(originalRequest: HttpRequest)(implicit g: SessionIdGenerator, s: SecretStoreApi) extends Session {
    lazy val id = g.next
    val tokens = Tokens(EmptyToken, EmptyServiceTokens)
  }

  case class ExistingSession(id: SessionId, originalRequest: HttpRequest, tokens: Tokens) extends Session

  implicit class SessionIdSerialize(val s: SessionId) extends AnyVal {
    def asString(implicit marshaller: SessionIdMarshaller): String =
      marshaller.encode(s)
  }

  implicit class SessionIdDeserialize(val s: String) extends AnyVal {
    def asSessionId(implicit marshaller: SessionIdMarshaller): Try[SessionId] =
      marshaller.decode(s)
  }

  object SecureSession extends SecureSessionIdComponent
                        with SessionIdExpiryComp
                        with SecretStoreComponent
                        with SessionStoreComponent {

    val cookieName = "border_session"
    val entropySize = Constants.SessionId.entropySize
    implicit val secretStore = InMemorySecretStore(Secrets(Current(currentExpiry), None))
    implicit val marshaller = SessionIdMarshaller(secretStore)
    implicit val generator: SessionIdGenerator = new SessionIdGenerator
    val sessionStore = new InMemorySessionStore
  }

  object Session {
    def sessionStore: SessionStoreApi = SecureSession.sessionStore
    implicit def generator: SessionIdGenerator = SecureSession.generator
    implicit def secretStore: SecretStoreApi = SecureSession.secretStore

    def apply(id: SessionId, originalRequest: HttpRequest, tokens: Tokens): Session =
      save(ExistingSession(id, originalRequest, tokens))

    def apply(s: String, originalRequest: HttpRequest): Session =
      sessionStore.get(s) getOrElse newSession(originalRequest)

    def apply(request: RoutedRequest): Session =
      request.borderCookie.flatMap(id => sessionStore.get(id)) getOrElse newSession(request.httpRequest)

    def newSession(originalRequest: HttpRequest): Session =
      save(NewSession(originalRequest))

    def save(session: Session): Session =
      sessionStore.update(session)
  }
}
