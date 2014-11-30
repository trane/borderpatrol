package com.lookout.borderpatrol

import java.security.{SecureRandom, Key, CryptoPrimitive, Timestamp}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import com.lookout.borderpatrol.sessions.SessionStore
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Response, Request, RequestProxy}
import com.twitter.util.{Future, Time}
import scala.util.{Try, Random}
import com.twitter.bijection._

object Session {

  trait Signer {
    val algo = "HmacSHA256"

    private def hmac(key: Key): Mac = {
      val m = Mac.getInstance(algo); m.init(key); m
    }

    def sign(key: Key, bytes: Array[Byte]): Array[Byte] = hmac(key).doFinal(bytes)
  }

  trait SessionIdBijector {
    implicit lazy val sessionId2Bytes: Bijection[SessionId, Array[Byte]] =
      new AbstractBijection[SessionId, Array[Byte]] {
        def apply(id: SessionId) = id.sigPayload ++ id.sig

        override def invert(bytes: Array[Byte]) = {
          if (bytes.size != SessionCrypto.sessionByteLength) throw new ClassCastException("not a session string")
          val (payload, sig) = bytes.splitAt(SessionCrypto.tsLength + SessionCrypto.dataLength)
          SessionCrypto.validateAsSecret(payload, sig) map { secret =>
            val (ts, data) = payload.splitAt(SessionCrypto.tsLength)
            new SessionId(Injection.long2BigEndian.invert(ts).get, data)(secret)
          } get
        }
      }
    implicit lazy val session2String = Injection.connect[SessionId, Array[Byte], Base64String, String]
  }

  trait Generator {
    private val random = new Random(new SecureRandom)

    private def nextByte = random.nextInt.toByte

    def data(n: Int): Array[Byte] = Array.fill(n)(nextByte)
  }

  object Secrets extends Generator {
    val secret1 = new Secret(data(16))
    val secret2 = new Secret(data(16))
  }

  trait SessionCrypto extends Signer with Generator with SessionIdBijector

  object SessionCrypto extends SessionCrypto {
    val sigLength = 32
    val dataLength = 16
    val tsLength = 8
    val sessionByteLength = tsLength + dataLength + sigLength
    val cookieDuration = Secret.rotationInterval * 2
    val secret1 = new Secret(data(dataLength))
    val secret2 = new Secret(data(dataLength))

    def currentExpiry: Long = Time.now.inLongSeconds + cookieDuration

    def encode(sessionId: SessionId): String = session2String(sessionId)

    def decode(s: String): Try[SessionId] = session2String.invert(s)

    def validateAsSecret(payload: Array[Byte], sig: Array[Byte]): Option[Secret] = {
      List(secret1, secret2).
      if (sign(secret1.key, payload) == sig) Some(secret1)
      else if (sign(secret2.key, payload) == sig) Some(secret2)
      else None
    }
  }

  case class SessionId(ts: Long = SessionCrypto.currentExpiry,
                       data: Array[Byte] = SessionCrypto.data(SessionCrypto.dataLength))(secret: Secret) {

    val sigPayload = Injection.long2BigEndian(ts) ++ data
    val sig = SessionCrypto.sign(secret.key, sigPayload)
    lazy val repr = SessionCrypto.encode(this)

    def valid(otherSig: Array[Byte]): Boolean =
      (sig == otherSig) &&
        (ts > Time.now.inLongSeconds && ts < SessionCrypto.currentExpiry) &&
        (data.size == SessionCrypto.dataLength)

    override def toString: String = repr
  }


  object Session {
    type Seconds = Long
    val expTime: Seconds = 60 * 60
    val expTimeTmp: Seconds = expTime * 24 * 7
  }


  /**
   * sessionId match {
   * SessionId(s, t) s == secret1 && t < Time.now => continue
   * _ => bail out
   * }
   *
   * Session(request).valid
   */

  object Secret {
    type Seconds = Long
    // defines how often secrets will be rotated
    val rotationInterval: Seconds = 60 * 60 * 24
    // lease lifetime (short-living)
    val leaseInterval: Seconds = 5
    // signature length - used for basic validation
    val hmacSha1SignLength = 20
    // number of random bytes generated for session ids
    val dataLength = 16
    // secret length - we keep 2 secrets in memory
    val keyLength = 64
    // stores the time when to check whether we need to rotate secrets
    val tsNextRefresh = -1

    /**
     * Deserialize the string into a Secret
     * @param string
     * @return
     */
    def deserialize(string: String): Secret = ???

    /**
     * Retrieve secrets, secret1 is current, secret2 is rotated
     * @return
     */
    def secrets: (Secret, Secret) = ???
  }

  case class Secret(data: Array[Byte], ts: Long = Time.now.inLongSeconds) {
    def encode(str: String, urlSafe: Boolean): Secret = ???

    def decode(secret: Secret): String = ???

    def serialize: String = ???

    def key: Key = new SecretKeySpec(data, SessionCrypto.algo)
  }

}

////  req.cookie.get {
///
/*
class Session {
  // internal session cookie lifetime to prevent unbound sessions
  val cookieLifetime = Secret.rotationInterval * 2
}
*/

// val sReq...
// val rReq ..
/// upstreamService(sReq.compose(rReq).http)
/// upstreamService(bpRequest.http)
trait BorderPatrolRequest[Req] {
  def http: Request
}

trait BorderReq extends RequestProxy {
  def authRequired: Boolean
}


trait SessionRequest[Req] {
  val request: Req
  val session: Session
}

/*
object SessionRequest {
  def compose(rReq: RoutedRequest): BorderPatrolRequest = {

  }

  case class Http(request: Request, session: Session) extends RequestProxy
}

trait RoutedRequest[Req] {
  val request: Req
  val service: String
  val destination: URL
  val authRequired: Boolean
}

object RoutedRequest {
  def compose(rReq: SessionRequest): BorderPatrolRequest = ???

  case class Http(request: Request) extends RequestProxy
}

class RoutedRequest(sessionRequest: SessionRequest) extends SessionRequest(sessionRequest.getRequest) {
  def toHttp: Request = {
    new Request(???)
  }
}

class SessionService[Req](store: SessionStore) extends Service[Request, SessionRequest] {
  type Sessioned = Req => BorderPatrolRequest

  val cookieKey = "border_session"
  def apply(req: Request): Future[SessionRequest] = {
    Future.value(new SessionRequest(req))
  }
}

class SessionFilter(sessionService: SessionService) extends Filter[Request, Response, SessionRequest, Response] {

  def apply(req: Request, service: Service[SessionRequest, Response]): Future[Response] =
    sessionService(req) flatMap { ss => service(ss) }
}
*/

