package com.lookout.borderpatrol.session

import com.twitter.util.Time

import scala.util.{Failure, Success, Try}

/**
 * This prototypes out an API for the SecretStore, keeping secrets in memory
 * which obviously doesn't work in a multi-server environment.
 *
 * Further work should be done to coordinate secrets among processes.
 *
 * For example, a zookeeper watcher could update current and previous in memory
 * on change, while an external service handles writing new secrets.
 */
sealed trait SecretStoreApi {
  def current: Current
  def previous: Option[Previous]
  def find(f: (Secret) => Boolean): Try[Secret]
}

case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
  import com.lookout.borderpatrol.session.SecretExpiry._
  private[this] var _secrets: Secrets = secrets

  def current = {
    val c = _secrets.current
    if (c.expiry > Time.now && c.expiry <= currentExpiry) c
    else {
      val c2 = Current(currentExpiry)
      _secrets = Secrets(c2, Some(Previous(c)))
      c2
    }
  }

  def previous = _secrets.previous

  def find(f: (Secret) => Boolean) =
    if (f(current)) Success(current)
    else previous match {
      case Some(p) if f(p) => Success(p)
      case _ => Failure(new Exception("No matching secrets found"))
    }
}

trait SecretStoreComponent {
  implicit val secretStore: SecretStoreApi
}
