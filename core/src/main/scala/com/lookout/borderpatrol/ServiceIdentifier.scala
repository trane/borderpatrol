package com.lookout.borderpatrol

import com.twitter.finagle.httpx.path.Path

/**
 * An identifier for Border Patrol to determine by `path` or
 * by `subdomain` which service a request should be routed to
 *
 * @param name The name that can be used to refer to a [[com.twitter.finagle.Name]]
 * @param path The external url path prefix that routes to the internal service
 * @param subdomain A default fall-back when path is only `/`
 * @param login The location to send a user when a request to this service is Unauthorized
 */
case class ServiceIdentifier(name: String, path: Path, subdomain: String, login: String)
