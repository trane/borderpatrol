package com.lookout.borderpatrol.routers

import com.twitter.finagle.httpx.path.Path

object models {

  /**
   * An identifier for Border Patrol to determine by `path` or
   * by `subdomain` which service a request should be routed to
   *
   * @example
   *          Let's say we have a service named "enterprise", if we
   *          define the instance of that as:
   *            {{{
   *            val ent = Service("enterprise", Path("/ent"), "default")
   *            val biz = Service("business", Path("/biz"), "api")
   *            }}}
   *
   *          The following urls would match the services:
   *            - api.example.com/ent => ent
   *            - default.example.com/ => ent
   *            - default.example.com/biz => biz
   *            - api.example.com => biz
   *            - api.example.com/ent => ent
   *          These would not match the services:
   *            - a.api.example.com => ???
   *            - example.com => ???
   *
   * @param name The service name used for authentication
   * @param path The external url path prefix that routes to the internal service
   * @param subdomain A default fall-back when path is only `/`
   */
  case class ServiceIdentifier(name: String, path: Path, subdomain: String)

}
