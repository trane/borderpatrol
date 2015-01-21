package com.lookout.borderpatrol.session

import com.twitter.util.{Throw, Time}
import scala.concurrent.duration.Duration
import scala.concurrent.{Future => ScalaFuture, Promise => ScalaPromise, Await}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import com.lookout.borderpatrol.session._
import concurrent.ExecutionContext.Implicits.global

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
  def current: Secret
  def previous: Secret
  def find(f: (Secret) => Boolean): Try[Secret]
}

case class ConsulSecretStore(watcher: SecretsWatcherApi) extends SecretStoreApi {
  //During initialization, we want this to be a hard failure that prevents server from starting
  private[this] var _secrets: Secrets = watcher.initialSecrets

  /**
   * Get Next secrets
   * @return
   */
  private def nextSecrets: Secrets = {
    watcher.getNext match {
      case Success(newSecrets) => newSecrets
      case Failure(f) => {
        //Do something indicating we got an error
        println(s"Unable to get new secrets: Exception $f")
        _secrets
      }
    }
  }

  def current = {
    val c = _secrets.current
    if (c.expiry > Time.now && c.expiry <= SecretExpiry.currentExpiry) c
    else {
      println("Secrets have expired")
      _secrets = nextSecrets
    }
    _secrets.current
  }

  def previous = _secrets.previous

  def find(f: (Secret) => Boolean) =
    if (f(current)) Success(current)
    else if (f(previous)) Success(previous)
    else Failure(new Exception("No matching secrets found"))
}

case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
  import com.lookout.borderpatrol.session.SecretExpiry._
  private[this] var _secrets: Secrets = secrets

  def current = {
    val c = _secrets.current
    if (c.expiry > Time.now && c.expiry <= currentExpiry) c
    else {
      val c2 = Secret(currentExpiry)
      _secrets = Secrets(c2, c)
      c2
    }
  }

  def previous = _secrets.previous

  def find(f: (Secret) => Boolean) =
    if (f(current)) Success(current)
    else if (f(previous)) Success(previous)
    else Failure(new Exception("No matching secrets found"))
}

trait SecretStoreComponent {
  implicit val secretStore: SecretStoreApi
}
