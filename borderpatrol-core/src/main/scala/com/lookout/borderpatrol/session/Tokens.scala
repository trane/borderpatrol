package com.lookout.borderpatrol.session

import argonaut.Argonaut._
import argonaut._

import scalaz.State

sealed trait Token

case object EmptyToken extends Token
case class MasterToken(value: String) extends Token
case class ServiceToken(name: String, value: String) extends Token

trait ServiceTokensBase {
  val services: Map[String,String]
  def get(name: String): Option[ServiceToken]
  def +(st: ServiceToken): ServiceTokensBase
  def ++(st: ServiceTokensBase): ServiceTokensBase
}

case object EmptyServiceTokens extends ServiceTokensBase {
  lazy val services = Map[String,String]()
  def get(name: String) = None
  def +(st: ServiceToken) = ServiceTokens(Map[String,String](st.name -> st.value))
  def ++(st: ServiceTokensBase) = st
}

case class ServiceTokens(services: Map[String, String]) extends ServiceTokensBase {
  def get(name: String) =
    services.get(name).map(v => ServiceToken(name, v))
  def +(st: ServiceToken): ServiceTokens =
    ServiceTokens(services + (st.name -> st.value))
  def ++(st: ServiceTokensBase): ServiceTokens =
    copy(services ++ st.services)
}

case class Tokens(master: Token, services: ServiceTokensBase) {
  def service(name: String): Option[ServiceToken] = services.get(name)
  def +=(token: Token): Tokens = TokenState(token).run(this)._2
  def ++=(tokens: ServiceTokensBase): Tokens = TokenState(tokens).run(this)._2
  def ++=(other: Tokens): Tokens = TokenState(other).run(this)._2
}

object TokenState {
  /**
   * Add master tokens to the current tokens
   * @param token A master token
   * @return
   */
  def apply(token: Token): State[Tokens, Tokens] = for {
    _ <- State.modify((cur: Tokens) => (token, cur) match {
      case (MasterToken(v), Tokens(MasterToken(w), _)) if v == w => cur
      case (t: MasterToken, _) => cur.copy(t, cur.services)
      case (t: ServiceToken, Tokens(_, st)) => cur.copy(cur.master, st + t)
      case _ => cur
    })
    s <- State.get
  } yield Tokens(s.master, s.services)

  /**
   * Add service tokens to the current tokens
   * @param tokens A master token
   * @return
   */
  def apply(tokens: ServiceTokensBase): State[Tokens, Tokens] = for {
    _ <- State.modify((cur: Tokens) => (tokens, cur) match {
      case (ServiceTokens(map), _) => cur.copy(cur.master, cur.services ++ tokens)
      case _ => cur
    })
    s <- State.get
  } yield Tokens(s.master, s.services)

  /**
   * Converge new tokens with the current tokens
   * @param tokens A master token
   * @return
   */
  def apply(tokens: Tokens): State[Tokens, Tokens] = for {
    _ <- State.modify((cur: Tokens) => (tokens, cur) match {
      case (Tokens(mt, st), _) => cur.copy(mt, cur.services ++ st)
    })
    s <- State.get
  } yield Tokens(s.master, s.services)
}

object TokenJson {

  implicit def MasterTokenCodecJson: CodecJson[MasterToken] =
    casecodec1(MasterToken.apply, MasterToken.unapply)("auth_service")

  implicit def ServiceTokensCodecJson: CodecJson[ServiceTokens] =
    casecodec1(ServiceTokens.apply, ServiceTokens.unapply)("service_tokens")

  implicit def TokenCodecJson: CodecJson[Token] =
    CodecJson(
      (t: Token) =>
        ("auth_service" := t.asInstanceOf[MasterToken].value) ->:
          jEmptyObject,
      c => for {
        value <- (c --\ "auth_service").as[String]
      } yield MasterToken(value))

  implicit def ServiceTokensBaseCodecJson: CodecJson[ServiceTokensBase] =
    CodecJson(
      (t: ServiceTokensBase) =>
        ("service_tokens" := t.services) ->:
          jEmptyObject,
      c => for {
        tokens <- (c --\ "auth_service").as[Map[String,String]]
      } yield ServiceTokens(tokens))

  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (ts: Tokens) =>
        ("auth_service" := ts.master.asInstanceOf[MasterToken].value) ->:
        ("service_tokens" := ts.services.services) ->:
          jEmptyObject,
      c => for {
        master <- (c --\ "auth_service").as[String]
        services <- (c --\ "service_tokens").as[Map[String, String]]
      } yield Tokens(MasterToken(master), ServiceTokens(services)))

  object MasterTokenJson {
    def apply(s: String) =
      s.decodeOption[MasterToken]
  }

  object ServiceTokensJson {
    def apply(s: String) =
      s.decodeOption[ServiceTokens]
  }

  object TokensJson {
    def apply(s: String) =
      s.decodeOption[Tokens]
  }

}
