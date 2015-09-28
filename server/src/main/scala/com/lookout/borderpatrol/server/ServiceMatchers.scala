package com.lookout.borderpatrol.server

import com.twitter.util.Future

import io.finch.route._

/**
 * This provides the Finch compatible [[io.finch.route.Router]] types for composing the logic behind Border Patrol's
 * service inference.
 *
 */
object ServiceMatchers {

  /**
   * Extrator for a default service
   *
   * @param f A function that takes a hostname and returns a serviceidentifier name
   */
  private[server] case class DefaultService(name: String, f: String => Option[String])
      extends Router[Option[String]] {

    import Router._

    override def apply(input: Input): Option[(Input, () => Future[Option[String]])] =
      for {
        h <- input.request.host
      } yield (input.drop(1), () => Future.value(f(h)))
  }
}
