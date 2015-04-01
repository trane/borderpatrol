package com.lookout.borderpatrol.session.secret

import java.util.concurrent.TimeoutException

import argonaut._
import com.lookout.borderpatrol.session.Secrets
import com.lookout.borderpatrol.session.secret.watcher.consul.ConsulService
import com.twitter.bijection.{Injection, Base64String}
import com.twitter.io.Charsets
import org.jboss.netty.handler.codec.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future => ScalaFuture, Promise => ScalaPromise}
import scala.util.Try
import com.twitter.bijection._

trait SecretsWatcherApi {
  def initialSecrets: Secrets

  def getNext: Try[Secrets]
}



