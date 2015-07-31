package com.lookout

import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.HttpHeaders
import com.twitter.finagle.httpx

package object borderpatrol {
  import auth._
  import sessionx._

  implicit class AnyOps[A](val any: A) extends AnyVal {
    def toFuture: Future[A] =
      Future.value[A](any)
    def toFailedFuture(e: Throwable): Future[A] =
      Future.exception(e)
  }

  implicit class IdxByteSeqOps(val bytes: IndexedSeq[Byte]) extends AnyVal {
    def as[A](implicit f: IndexedSeq[Byte] => A): A =
      f(bytes)
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def as[A](implicit f: String => A): A =
      f(s)
  }

  object view {

    trait View[A, B] {
      def apply(a: A): B
    }

    object View {
      def apply[A, B](f: A => B): View[A, B] = new View[A, B] {
        def apply(a: A): B =
          f(a)
      }
      implicit def identity[A]: View[A, A] =
        View(a => a)
    }

  }

  import view._
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
