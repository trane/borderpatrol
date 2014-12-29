package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit
import com.twitter.util.{Future, Await, Duration}
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

    def apply(request: HttpRequest): Session = NewSession(request)

    def apply(s: String, request: HttpRequest): Session =
      sessionStore.get(s) getOrElse NewSession(request)
  }
}
