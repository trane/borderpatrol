package com.lookout.borderpatrol.sessionx

/**
 * Tag is a byte that help us differentiate the set of SessionId from others.
 */
trait Tag {
  val id: Byte
}
case object Untagged extends Tag {
  val id = 0.toByte
}
case object AuthenticatedTag extends Tag {
  val id = 1.toByte
}

// create a smart constructor
object Tag {
  def apply(id: Byte): Tag = id match {
    case 1 => AuthenticatedTag
    case _ => Untagged
  }


  // here is a partial function that acts like a finagle filter
  val authenticated: PartialFunction[Tag, Boolean] = {
    case AuthenticatedTag => true
    case _ => false
  }
}
