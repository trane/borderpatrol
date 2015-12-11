package com.lookout.borderpatrol

import scala.util.{Failure, Success, Try}

package object server {

  implicit class StringOps(val s: String) extends AnyVal {
    def getOrDefault(defaultValue: String): String =
      s match {
        // scalastyle:off null
        case null | "" => defaultValue
        case x => x
      }
  }

  implicit class TryOps[A](val tryA: Try[A]) extends AnyVal {
    def getOrDefault(default: A): A = tryA match {
      case Success(a) => a
      case Failure(e) => default
    }
  }
}
