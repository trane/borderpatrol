package com.lookout.borderpatrol.util

object Combinators {
  /**
   * Handler for objects that require side-effects for initialization
   * @param a The object
   * @param f Side effect(s)
   * @tparam A
   * @return The object
   */
  def tap[A](a: A)(f: A => Unit): A = { f(a); a }
}
