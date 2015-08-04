package com.lookout

import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.HttpHeaders
import com.twitter.finagle.httpx

/**
 * This is the root package of borderpatrol-core which provides a functional approach to web sessions and
 * authentication built on top of [[com.twitter.finagle Finagle]]. It contains two main packages:
 * [[com.lookout.borderpatrol.sessionx]] and [[com.lookout.borderpatrol.auth]] which contain types and functions to
 * interact with HTTP services.
 *
 *
 */
package object borderpatrol {
  import auth._
  import sessionx._

  /**
   * Wraps any object with a `toFuture` method
   *
   * @param any object to be altered
   *
   * @tparam A object type
   */
  implicit class AnyOps[A](val any: A) extends AnyVal {

    /**
     * Wraps object into `Future`
     * @return
     */
    def toFuture: Future[A] = Future.value[A](any)
  }

  /**
   *
   * Wraps any `Throwable` with a `toFutureException` method
   * @param t throwable to wrap
   */
  implicit class ThrowableOps(val t: Throwable) extends AnyVal {

    /**
     * Wraps `Throwable` in a `Future` exception
     * @tparam A
     * @return
     */
    def toFutureException[A]: Future[A] = Future.exception[A](t)
  }

  implicit class IdxByteSeqOps(val bytes: IndexedSeq[Byte]) extends AnyVal {
    def as[A](implicit f: IndexedSeq[Byte] => A): A =
      f(bytes)
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def as[A](implicit f: String => A): A =
      f(s)
  }

  /**
   * Contains the trait and companion object for explicit "views" from `A => B`
   */
  object view {

    /**
     * Express transformations explicitly as some view from `A` to `B`, since Scala's "view" bounds are deprecated
     */
    trait View[A, B] {
      def apply(a: A): B
    }

    /**
     * Helper object for creating Views
     */
    object View {

      /**
       * Create a View[A,B] with a function from `A => B`
       *
       * @param f transformation that should succeed
       * @tparam A from type
       * @tparam B to type
       * @return a view from A to B
       */
      def apply[A, B](f: A => B): View[A, B] = new View[A, B] {
        def apply(a: A): B =
          f(a)
      }

      /**
       * Handles when a View[A, A] is created
       * @tparam A
       * @return
       */
      implicit def identity[A]: View[A, A] =
        View(a => a)
    }

  }

  import view._

  /** shorthand notation Thing %> Other */
  type %>[A, B] = View[A, B]


  object errors {
    abstract class BorderError(val status: httpx.Status, val description: String) extends Exception
    class InvalidRequest(description: String = "") extends BorderError(httpx.Status.BadRequest, description)
    class UnauthorizedRequest(description: String = "") extends BorderError(httpx.Status.Unauthorized, description)
    class ForbiddenRequest(description: String = "") extends BorderError(httpx.Status.Forbidden, description)
  }

  object request {

    trait RequestBase {
      val request: httpx.Request
      val auth: Option[String] = request.headerMap.get(HttpHeaders.Names.AUTHORIZATION)
    }

    case class AuthRequest[A](request: httpx.Request) extends RequestBase

    case class AuthResourceRequest[A](request: httpx.Request) extends RequestBase

    trait BorderRequest[A] {
      val authInfo: AuthInfo[A]
      val request: httpx.Request
      val sessionId: SessionId
    }

  }

}
