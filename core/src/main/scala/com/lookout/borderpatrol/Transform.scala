package com.lookout.borderpatrol

/**
 * Abstraction for those that are directing requests directly to the Identity Provider
 */
trait Transform[A, B] {
  def apply(a: A): B
}


