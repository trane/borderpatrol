package com.lookout.borderpatrol

package object server {

  implicit class StringOps(val s: String) extends AnyVal {
    def getOrDefault(defaultValue: String): String =
      s match {
        case null | "" => defaultValue
        case x => x
      }
  }
}
