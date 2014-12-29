package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._

import scalaz.State

sealed trait Token {
  val value: String
}

case object EmptyToken extends Token {
  val value = ""
}
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
  def +=(token: ServiceToken): Tokens = TokenState(token).run(this)._2
  def +=(token: MasterToken): Tokens = TokenState(token).run(this)._2
  def ++=(other: Tokens): Tokens = TokenState(other).run(this)._2
  def ++=(tokens: ServiceTokensBase): Tokens = TokenState(tokens).run(this)._2
}

object TokenState {
  /**
   * Add master tokens to the current tokens
   * @param token A master token
   * @return
   */
  def apply(token: Token): State[Tokens, Tokens] = for {
    _ <- State.modify((m: Tokens) => (token, m) match {
      case (MasterToken(_), Tokens(MasterToken(_), _)) => m
      case (ServiceToken(n, v), Tokens(_, st)) => m.copy(m.master, st + ServiceToken(n, v))
      case (MasterToken(v), _) => m.copy(MasterToken(v), m.services)
      case _ => m
    })
    s <- State.get
  } yield Tokens(s.master, s.services)

  /**
   * Add service tokens to the current tokens
   * @param tokens A master token
   * @return
   */
  def apply(tokens: ServiceTokensBase): State[Tokens, Tokens] = for {
    _ <- State.modify((m: Tokens) => (tokens, m) match {
      case (ServiceTokens(map), _) => m.copy(m.master, m.services ++ tokens)
      case _ => m
    })
    s <- State.get
  } yield Tokens(s.master, s.services)

  /**
   * Converge new tokens with the current tokens
   * @param tokens A master token
   * @return
   */
  def apply(tokens: Tokens): State[Tokens, Tokens] = for {
    _ <- State.modify((m: Tokens) => m match {
      case Tokens(mt, st) => m.copy(mt, m.services ++ st)
      case _ => m
    })
    s <- State.get
  } yield Tokens(s.master, s.services)
}

object TokenJson {

  implicit def MasterTokenCodecJson: CodecJson[MasterToken] =
    casecodec1(MasterToken.apply, MasterToken.unapply)("auth_tokens")

  implicit def ServiceTokensCodecJson: CodecJson[ServiceTokens] =
    casecodec1(ServiceTokens.apply, ServiceTokens.unapply)("service_tokens")

  implicit def TokenCodecJson: CodecJson[Token] =
    CodecJson(
      (t: Token) =>
        ("auth_service" := t.value) ->:
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
        ("auth_service" := ts.master) ->:
        ("service_tokens" := ts.services) ->:
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
