package com.lookout.borderpatrol

import com.twitter.finagle.http.Request
import com.twitter.finagle.http.path.Path

/*
 * We derive a service `name` (a [[String]] name referencing a [[com.twitter.finagle.Name Name]]) via the `Path`
 * and the subdomain of the host.
 *
 *  In the Request url `http://:customer.example.com/:service/rest/of/path`
 *  => extract `:customer`, lookup a matching `CustomerIdentifier`
 *  => extract `:service`, then lookup a matching `ServiceIdentifier`.
 *     If `:service` matches the LoginManger path(s) in `CustomerIdentifier`, then use the default `ServiceIdentifier`
 *
 * @example
 *          Let's say we have a service named "enterprise", if we
 *          define the instance of that as:
 *            {{{
 *            val ent = ServiceIdentifier(Path("/ent"))
 *            val biz = ServiceIdentifier(Path("/biz"))
 *
 *            val customer1 = CustomerIdentifier("good", ent, LoginManager(Path("check"), Path("confirm")))
 *            }}}
 *
 *          The following urls would match the services:
 *            - good.example.com/ent => (customer1, ent)
 *            - good.example.com/api => (customer1, biz)
 *            - good.example.com/check => (customer1, ent)
 *            - good.example.com/confirm => (customer1, ent)
 *          These would not match the services:
 *            - what.example.com => None
 *            - example.com => None
 *            - good.example.com/ => None
 */
case class ServiceMatcher(customerIds: Set[CustomerIdentifier], serviceIds: Set[ServiceIdentifier]) {

  val domainTerm = "."

  /**
   * Helper for finding longest subdomain prefix in a set
   * @param sis A set of identifiers that have matched some precondition
   * @param cmp A comparable function
   * @return The maximuma of folding over the set with the cmp function
   */
  private[this] def foldWith[A](sis: Set[A], cmp: (A, A) => A): Option[A] =
    sis.foldRight(Option.empty[A])((lhs, res) => res match {
      case Some(rhs) => Some(cmp(lhs, rhs))
      case None => Some(lhs)
    })

  /**
   * Gives the name of the Customer that best matches the subdomain of the host string, or None
   *
   * @param host The fully qualified host name
   * @return the service name from the longest matching subdomain
   */
  def subdomain(host: String): Option[CustomerIdentifier] =
    foldWith(
      customerIds.filter(ci => host.startsWith(ci.subdomain + domainTerm)),
      (cid1: CustomerIdentifier, cid2: CustomerIdentifier) =>
        if (cid1.subdomain.size > cid2.subdomain.size) cid1 else cid2
    )

  /**
   * Find the longest matching path in the request
   *
   * @example
   *          Given a request of path of "/a" and a set of paths Set("/account", "/a")
   * @param path path from request
   * @return the service name from the longest matching path
   */
  def path(path: Path): Option[ServiceIdentifier] =
    foldWith(
      serviceIds.filter(sid => path.startsWith(sid.path)),
      (sid1: ServiceIdentifier, sid2: ServiceIdentifier) =>
        if (sid1.path.toString.size > sid2.path.toString.size) sid1 else sid2
    )

  /**
   * Derive a CustomerIdentifier and ServiceIdentifier from an `http.Request`
   * - Find CustomerIdentifier from `subdomain` from `req.host`
   * - Find ServiceIdentifier from `req.path`.
   *   If it fails to find, then check if it matches with paths in LoginManager. If it does,
   *   then use default ServiceIdentifier
   */
  def get(req: Request): Option[(CustomerIdentifier, ServiceIdentifier)] =
    (req.host.flatMap(subdomain), path(Path(req.path))) match {
      case (Some(cid), Some(sid)) => Some((cid, sid))
      case (Some(cid), None) if (cid.isLoginManagerPath(Path(req.path))) => Some((cid, cid.defaultServiceId))
      case _ => None
    }
}

