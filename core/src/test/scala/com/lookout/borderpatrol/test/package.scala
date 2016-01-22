package com.lookout.borderpatrol

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Status, Request, Response}
import com.twitter.util.{Await, Future}

package object test {

  /**
   * Helper ops for Futures in testing
   */
  implicit class FutureOps[A](val future: Future[A]) extends AnyVal {
    /**
     * Force results for a future
     */
    def results: A =
      Await.result(future)

    /**
     * Determine if the Future failed
     */
    def isThrowable: Boolean =
      Await.ready(future).poll.get.isThrow
  }

  def testService(f: Request => Boolean): Service[Request, Response] = Service.mk[Request, Response](r => {
    val res = if (f(r)) Response(Status.Ok)
              else Response(Status.BadRequest)
    res.contentString = r.contentString
    Future.value(res)
  })
}
