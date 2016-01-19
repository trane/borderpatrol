package com.lookout.borderpatrol.server

import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol._
import com.lookout.borderpatrol.sessionx.{SessionStore, SecretStoreApi}
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.service.RoutingService
import com.twitter.finagle.httpx.{Request, Response}
import com.twitter.finagle.stats.StatsReceiver


object services {

  /**
   * Get IdentityProvider map of name -> Service chain
   *
   * As of now, we only support `keymaster` as an Identity Provider
   */
  def identityProviderChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
      Map[String, Service[SessionIdRequest, Response]] =
    Map("keymaster" -> keymasterIdentityProviderChain(sessionStore))

  /**
   * Get AccessIssuer map of name -> Service chain
   *
   * As of now, we only support `keymaster` as an Access Issuer
   */
  def accessIssuerChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
      Map[String, Service[SessionIdRequest, Response]] =
    Map("keymaster" -> keymasterAccessIssuerChain(sessionStore))

  /**
   * The sole entry point for all service chains
   */
  def MainServiceChain(implicit config: ServerConfig, statsReceiver: StatsReceiver): Service[Request, Response] = {
    implicit val secretStore = config.secretStore
    val serviceMatcher = ServiceMatcher(config.customerIdentifiers, config.serviceIdentifiers)

    RoutingService.byPath {
      case "/logout" =>
        ExceptionFilter() andThen /* Convert exceptions to responses */
          ServiceFilter(serviceMatcher) andThen /* Validate that its our service */
          LogoutService(config.sessionStore)
      case _ =>
        ExceptionFilter() andThen /* Convert exceptions to responses */
          ServiceFilter(serviceMatcher) andThen /* Validate that its our service */
          SessionIdFilter(config.sessionStore) andThen /* Get or allocate Session/SessionId */
          BorderService(identityProviderChainMap(config.sessionStore),
          accessIssuerChainMap(config.sessionStore)) /* Glue that connects to identity & access service */
    }
  }
}
