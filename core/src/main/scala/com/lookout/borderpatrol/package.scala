package com.lookout

import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.HttpHeaders
import com.twitter.finagle.http

/**
 * This is the root package of borderpatrol-core which provides a functional approach to web sessions and
 * authentication built on top of [[com.twitter.finagle Finagle]]. It contains two main packages:
 * [[com.lookout.borderpatrol.sessionx]] and [[com.lookout.borderpatrol.auth]] which contain types and functions to
 * interact with HTTP services.
 *
 *
 */
package object borderpatrol {

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

  object errors {
    abstract class BorderError(val status: http.Status, val description: String) extends Exception
    class InvalidRequest(description: String = "") extends BorderError(http.Status.BadRequest, description)
    class UnauthorizedRequest(description: String = "") extends BorderError(http.Status.Unauthorized, description)
    class ForbiddenRequest(description: String = "") extends BorderError(http.Status.Forbidden, description)
  }

  object request {

    trait RequestBase {
      val request: http.Request
      val auth: Option[String] = request.headerMap.get(HttpHeaders.Names.AUTHORIZATION)
    }

    case class AuthRequest[A](request: http.Request) extends RequestBase

    case class AuthResourceRequest[A](request: http.Request) extends RequestBase

  }

}
