package com.lookout.borderpatrol.session

import com.lookout.borderpatrol.session.tokens.TokenState

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
  def get(name: String) =
    None
  def +(st: ServiceToken) =
    ServiceTokens(Map[String,String](st.name -> st.value))
  def ++(st: ServiceTokensBase) =
    st
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
  def service(name: String): Option[ServiceToken] =
    services.get(name)
  def +=(token: Token): Tokens =
    TokenState(token).run(this)._2
  def ++=(tokens: ServiceTokensBase): Tokens =
    TokenState(tokens).run(this)._2
  def ++=(other: Tokens): Tokens =
    TokenState(other).run(this)._2
}

object Tokens {
  def empty: Tokens = Tokens(EmptyToken, EmptyServiceTokens)
}




