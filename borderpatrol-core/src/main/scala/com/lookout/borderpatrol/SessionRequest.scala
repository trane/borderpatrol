package com.lookout.borderpatrol

import java.security.{SecureRandom, Key, CryptoPrimitive, Timestamp}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import com.lookout.borderpatrol.Signer
import com.lookout.borderpatrol.sessions.SessionStore
import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Response, Request, RequestProxy}
import com.twitter.util.{Future, Time}
import scala.util.{Failure, Success, Try, Random}
import com.twitter.bijection._
// trait Token
// type Token = String
// case class Tokens(mt: MasterToken, sts: Option[Map[ServiceToken]])
// case class MasterToken(token: Token)
// case class ServiceToken(service: String, token: Token)
// object MasterToken {
//  implicit def MasterTokenCodeJson: CodecJson[MasterToken] =
//    casecodec1(MasterToken.apply, MasterToken.unapply)("auth_service")
// }
// object ServiceToken {
//   implicit def ServiceTokenCodecJson: CodeJson[ServiceToken] =
//     casecodec2(ServiceToken.apply, ServiceToken.unapply)("


// object Token {
//   def apply(s: String): Option[List[Token]] =
//     val json = JSON.parse(s)
//     for {
//       (k, v) <- json
//       (mt, stList) <- v
//     }

// trait Token {
//   type MasterToken = String
//   type ServiceToken = String
// }

//
// (Request, Session, Option[MasterToken], Option[ServiceToken])
object Session {
  type Token = String
  type MasterToken = Token
  type ServiceToken = Token

  def thing(mt: Option[MasterToken]) = ???



  trait SessionIdBijector {
    // Injection(thing)
    // Injection(thing)(session2String)
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
      if (sign(secret1.key, payload) == sig) Some(secret1)
      else if (sign(secret2.key, payload) == sig) Some(secret2)
      else None
    }
    def validateWithSecret(t: BytesTuple)(s: Secret): Boolean =
      sign(s.key, t._1 ++ t._2) == t._3
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
  //val session: Session
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
/*
import com.lookout.borderpatrol._
*/
import argonaut._
import Argonaut._

object Stuff {
  type Token = String
  type Service = String

  val i = """{"auth_service": "abcd", "service_tokens": {"service1": "jk", "service2": "ep"}}"""

  case class SToken(service: Service, token: Token)

  case class STokenMap(service_tokens: Map[String, Token])

  case class MToken(auth_service: Token)

  case class Tokens(auth_service: String, service_tokens: Map[String, String]) {
    // def apply(auth_service: String, service_tokens: Map[String, String]) = {
    //   SessionTokens(MToken(auth_service), (service_tokens map (t => SToken(t._1, t._2))).toSet
    // }
  }

  case class SessionTokens(masterToken: MToken, serviceTokens: Set[SToken])

  case class STokenSet(service_tokens: Set[SToken])

  implicit def MTokenCodecJson: CodecJson[MToken] = casecodec1(MToken.apply, MToken.unapply)("auth_service")

  implicit def STokenMapCodecJson: CodecJson[STokenMap] =
    casecodec1(STokenMap.apply, STokenMap.unapply)("service_tokens")

  implicit def TokensCodecJson: CodecJson[Tokens] = casecodec2(Tokens.apply, Tokens.unapply)("auth_service", "service_tokens")

  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (t: Tokens) =>
        ("auth_service" := t.auth_service) ->:
          ("service_tokens" := t.service_tokens) ->:
          jEmptyObject,
      c => for {
        as <- (c --\ "auth_service").as[String]
        st <- (c --\ "service_tokens").as[Map[String, String]]
      } yield Tokens(as, st))

  i.decodeOption[Tokens]

  implicit def STokenCodecJson: CodecJson[SToken] =
    CodecJson(
      (st: SToken) =>
        (st.service := st.token) ->:
          jEmptyObject,
      c => for {
        m <- (c --\ jString).as[Map[String, String]]
        (k, v) <- m
      } yield SToken(k, v))

  implicit def STokenMapEncodeJson: EncodeJson[STokenMap] =
    EncodeJson((stm: STokenMap) => ("service_tokens" := stm.service_tokens)) ->: jEmptyObject

  implicit def STokenMapDecodeJson: DecodeJson[STokenMap] =
    DecodeJson(c => for {
      mp <- (c --\ "service_tokens").as[STokenMap]
    } yield mp)
}

