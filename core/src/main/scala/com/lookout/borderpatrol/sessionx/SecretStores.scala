package com.lookout.borderpatrol.sessionx


import com.twitter.util._
import com.twitter.finagle.Httpx



/**
 * Two secrets must be in rotation at any given time:
 * - Current: used for creating new sessions and validating incoming non-expired sessions
 * - Previous: validating incoming non-expired sessions, e.g. sessions signed by yesterday's key
 *
 * Since each [[com.lookout.borderpatrol.sessionx.Secret Secret]] expires (default of 1 day), the
 * window of a non-expired [[com.lookout.borderpatrol.sessionx.Session Session]] is somewhere between the expiry of
 * the current `Secret` and the previous `Secret`
 */
trait SecretStoreApi {
  def current: Secret

  def previous: Secret

  def find(f: Secret => Boolean): Option[Secret]
}

/**
 * Default implementations of [[com.lookout.borderpatrol.sessionx.SecretStoreApi SecretStoreApi]]
 */
object SecretStores {
  val ConsulSecretsKey = "secretStore/secrets"

  /**
   * A useful [[com.lookout.borderpatrol.sessionx.Secrets Secrets]] mock store for quickly testing and prototyping
   *
   * @param secrets the current secret and previous secret
   */
  case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Var")) // this is for mocking
    private[this] var _secrets: Secrets = secrets

    def current: Secret = {
      val c = _secrets.current
      if (c.expired) {
        _secrets = _secrets.copy(Secret(), c)
        _secrets.current
      }
      else c
    }

    def previous: Secret =
      _secrets.previous

    def find(f: (Secret) => Boolean): Option[Secret] =
      if (f(current)) Some(current)
      else if (f(previous)) Some(previous)
      else None

  }

  /**
   * A store to access the current and previous [[com.lookout.borderpatrol.sessionx.Secret]] stored in the consul
   * server.
   * It will use a in memory cache before making a request to the server
   *
   * @param consul An instance of [[com.lookout.borderpatrol.sessionx.ConsulConnection]] to make requests
   *               to the consul server
   * @param poll How often in seconds to check for updated Secrets on the consul server
   */
  case class ConsulSecretStore(consul:  ConsulConnection, poll: Long, secretsStart: Secrets) extends SecretStoreApi {
    val cache = ConsulSecretCache(poll, consul, secretsStart)
    new Thread(cache).start()

    /**
     * Get the current secret from the cache layer
     */
    def current: Secret =
      cache.secrets.current

    /**
     * Get the previous secret from the cache layer
     */
    def previous: Secret =
      cache.secrets.previous

    /**
     * Look for the Secret being checked in the function. Checks if this exists in the cache layer
     * If it is found only existing in consul and not in current or previous a cache rotation will be triggered
     * Returning None suggests the servers are extremely out of sync with each other or the connection to consul has
     * failed
     */
    def find(f: Secret => Boolean): Option[Secret] =
      cache.find(f)

  }

  object ConsulSecretStore {
    /**
     * Create a ConsulSecretStore to use.
     *
     * @param consulUrl the host name of the server
     * @param consulPort the port the kv store is listening on. Consul default is 8500
     * @param poll How often to check for updates on the consul server
     */
    def apply(consulUrl: String, consulPort: String, poll: Int): ConsulSecretStore = {
      val apiUrl = s"$consulUrl:$consulPort"
      val client = Httpx.newService(apiUrl)
      val consulConnection = new ConsulConnection(client, consulUrl, consulPort)
      val secretsDefault = Secrets(Secret(Time.fromMilliseconds(0)), Secret())
      new ConsulSecretStore(consulConnection, poll, secretsDefault)
    }

    /**
     * A constructor to define the starting secrets. Create a ConsulSecretStore to use.
     *
     * @param consulUrl the host name of the server
     * @param consulPort the port the kv store is listening on. Consul default is 8500
     * @param poll How often to check for updates on the consul server
     * @param secrets Initial secrets in the cache
     */
    def apply(consulUrl: String, consulPort: String, poll: Int, secrets: Secrets): ConsulSecretStore = {
      val apiUrl = s"$consulUrl:$consulPort"
      val client = Httpx.newService(apiUrl)
      val consulConnection = new ConsulConnection(client, consulUrl, consulPort)
      new ConsulSecretStore(consulConnection, poll, secrets)
    }
  }

}
