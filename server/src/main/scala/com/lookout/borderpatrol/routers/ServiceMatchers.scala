package com.lookout.borderpatrol.routers

import com.lookout.borderpatrol.routers.models.ServiceIdentifier
import com.twitter.finagle.httpx.path.Path
import com.twitter.util.{Try, Future}
import io.finch.route._

object ServiceMatchers {

  /**
   * Extrator for a default service
   *
   * @param f A function that takes a hostname and returns a serviceidentifier name
   */
  private[routers] case class DefaultService(name: String, f: String => Option[String])
      extends Router[Option[String]] {

    import Router._

    override def apply(input: Input): Option[(Input, () => Future[Option[String]])] =
      for {
        h <- input.request.host
      } yield (input.drop(1), () => Future.value(f(h)))
  }

  /**
   * Provides the functions for Finch's Router/Extractor which require
   * a signature of `String => Option[String]`
   */
  object ServicesMatcher {

    val domainTerm = "."
    val pathTerm = "/"

    /**
     * Find the longest matching subdomain that matches the host
     *
     * @example
     *          Given a host of "sub.subdomain.example.org" and a Set[ServiceIdentifier] of
     *            (ServiceIdentifier("one", "/s", "sub.subdomain"),
     *             ServiceIdentifier("two", "/s2", "sub"))
   *             return the ServiceIdentifier named "one" because it is the longest matching
     * @param host The fully qualified host name
     * @return the service name from the longest matching subdomain
     */
    def subdomain(host: String)(implicit services: Set[ServiceIdentifier]): Option[String] =
      services.filter(si => host.startsWith(si.subdomain + domainTerm))
              .foldRight(Option.empty[ServiceIdentifier])((si, res) => res match {
                case Some(s) if si.subdomain.size < s.subdomain.size => Some(s)
                case _ => Some(si)})
              .map(_.name)

    /**
     * Find the path exactly matching the path in the request
     *
     * @param pathString path string from request
     * @return the service name from the exact path match
     */
    def path(pathString: String)(implicit services: Set[ServiceIdentifier]): Option[String] = {
      val path = Path(pathString)
      services.find(_.path == path).map(_.name)
    }

  }

}
