package com.lookout.borderpatrol.routers

/**
 * This houses the two dependencies that Border Patrol has on authenticating requests
 *  - AuthService: a service to route authentication requests, e.g. oauth2 tokens
 *  - LoginService: a service to handle logins
 */
object services {

  case class AuthService

  case class LoginService

}
