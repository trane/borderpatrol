package com.lookout.borderpatrol

package object server {

  implicit class StringOps(val s: String) extends AnyVal {
    def getOrDefault(defaultValue: String): String =
      s match {
        // scalastyle:off null
        case null | "" => defaultValue
        case x => x
      }
  }
}
