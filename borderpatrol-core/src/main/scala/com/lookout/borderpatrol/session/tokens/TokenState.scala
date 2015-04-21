package com.lookout.borderpatrol.session.tokens

import com.lookout.borderpatrol.session.{Token, Tokens, MasterToken, ServiceToken, ServiceTokensBase, ServiceTokens}

import scalaz.State

object TokenState {
  /**
   * Add master data to the current data
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
   * Add service data to the current data
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
   * Converge new data with the current data
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
