package com.lookout.borderpatrol

import java.net.URL

import com.twitter.finagle.http.path.Path

/**
 * An identifier for Border Patrol to determine by `path` which service a request
 * should be routed to
 *
 * @param name The name that can be used to refer to a [[com.twitter.finagle.Name]]
 * @param hosts The list of URLs to upstream service
 * @param path The external url path prefix that routes to the internal service
 * @param rewritePath The (optional) internal url path prefix to the internal service. If present,
 *                    it replaces the external path in the Request URI
 */
case class ServiceIdentifier(name: String, hosts: Set[URL], path: Path, rewritePath: Option[Path]) {
  def isServicePath(p: Path): Boolean =
    p.startsWith(path)
}

/**
 * An identifier for Border Patrol to determine by `subdomain` which service a request
 * should be routed to
 *
 * @param subdomain
 * @param defaultServiceId
 * @param loginManager
 */
case class CustomerIdentifier(subdomain: String, defaultServiceId: ServiceIdentifier, loginManager: LoginManager) {
  def isLoginManagerPath(p: Path): Boolean =
    loginManager.protoManager.isMatchingPath(p)
}
