package com.lookout.borderpatrol.sessionx

import com.twitter.finagle.stats.StatsReceiver
import com.twitter.logging.Level
import com.twitter.util.Future

/**
 * A container for some type `A` with a unique identifier of `SignedId`
 */
trait Session[+A] { self =>
  val id: SignedId
  val data: A

  /**
   * Transform a `Session[A]` to a `Session[B]` given a function from `A => B`, keeping the same id
   */
  def map[B](f: A => B): Session[B] =
    Session(id, f(data))
}

object Session {

  /**
   * Primary method of recreating `Session[A]` from a given `SignedId` and data type `A`
   *
   * {{{
   *   case class Foo(i: Int)
   *
   *   val id = Await.result(SignedId.next)
   *   val data = Foo(42)
   *   val s = Session(id, data)
   *   val s2 = Session(id, data)
   *   s == s2 // true
   *   s === s2
   * }}}
   *
   * @param i the SignedId
   * @param d an arbitrary data value
   * @tparam A
   * @return a Session
   */
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Any"))
  def apply[A](i: SignedId, d: A): Session[A] =
    new Session[A] {
      override val id: SignedId = i
      override val data: A = d

      @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.AsInstanceOf"))
      def membersCanEqual(o: Any): Boolean = {
        val s: Session[_] = o.asInstanceOf[Session[_]]
        s.id == id && s.data == data
      }

      @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.IsInstanceOf"))
      // scalastyle:off null
      def canEqual(o: Any): Boolean =
        o != null && o.isInstanceOf[Session[_]]

      override def equals(o: Any): Boolean =
        canEqual(o) && membersCanEqual(o)

      override def toString: String =
        s"Session($id, $data)"

      override def hashCode: Int =
        41 * (id.hashCode() + 41) + data.hashCode()
    }

  /**
   * Mechanism for generating new [[Session]], returning a `Future` of the `Session[A]`
   *
   * {{{
   *   val data = 1
   *   val sessionFuture = for {
   *    s <- Session(data)
   *    _ <- SessionStore.InMemoryStore.update(s)
   *   } yield s
   * }}}
   *
   * @param data value you want to store
   * @param store the secret store to fetch current secret
   * @tparam A
   * @return Session
   */
  def apply[A](data: A, tag: Tag = Untagged)(implicit store: SecretStoreApi): Future[Session[A]] =
    if (store.current.expired) new SessionCreateUnavailable("only expired secrets available").toFutureException
    else {
      tag match {
        case AuthenticatedTag => SignedId.authenticated map (Session(_, data))
        case Untagged => SignedId.untagged map (Session(_, data))
        case _ => new SessionError("Invalid SignedId Tag found").toFutureException
      }
    }
}

