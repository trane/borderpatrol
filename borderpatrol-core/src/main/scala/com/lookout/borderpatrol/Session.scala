package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.id.{SignerComponent, ExpiryComponent, Marshaller, Generator}
import com.lookout.borderpatrol.session.secret._
import com.lookout.borderpatrol.session.store.{InMemorySessionStore, SessionStoreComponent}
import com.lookout.borderpatrol.session._
import com.twitter.util.Time
import com.twitter.finagle.httpx
import com.twitter.finagle.httpx.netty.Bijections
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyRequest}

trait SecureSession[+R, +A] {
  val id: SessionId
  val request: R
  val data: A
}

case class Session(id: SessionId, request: httpx.Request, data: Tokens) extends SecureSession[httpx.Request, Tokens] {
  def equals[R <: httpx.Request, A <: Tokens](o: SecureSession[R, A]): Boolean =
    id == o.id && data == o.data && (request.uri == o.request.uri &&
      request.method == o.request.method)
}

object Session extends SignerComponent
    with ExpiryComponent
    with SecretStoreComponent
    with SessionStoreComponent {

  val cookieName = "border_session"
  val entropySize = Constants.SessionId.entropySize
  implicit val secretStore = getSecretStore
  implicit val marshaller = Marshaller(secretStore)
  implicit val generator: Generator = new Generator
  val sessionStore = new InMemorySessionStore

  def apply(request: httpx.Request): Session =
    request.cookies.getValue(cookieName).flatMap(id => sessionStore.get(id)) getOrElse newSession(request)

  def apply(key: String, httpRequest: NettyRequest): Session =
    Session(key, Bijections.requestFromNetty(httpRequest))

  def apply(key: String, request: httpx.Request): Session =
    sessionStore.get(key) getOrElse newSession(request)

  def newSession(request: httpx.Request): Session =
    Session(generator.next, request, Tokens.empty)

  def save(session: Session): Session =
    sessionStore.update(session)

  //TODO: This should be configurable(should be Memory for unit tests, and consul in run mode
  //def getSecretStore: SecretStoreApi = ConsulSecretStore(ConsulSecretsWatcher(new ConsulService))
  def getSecretStore: SecretStoreApi = InMemorySecretStore(Secrets(Secret(SecretExpiry.currentExpiry), Secret(Time.fromSeconds(100))))
}
