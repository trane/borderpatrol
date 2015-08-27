package com.lookout.borderpatrol

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
}
