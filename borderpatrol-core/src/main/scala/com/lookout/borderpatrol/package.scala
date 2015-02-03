package com.lookout

import java.io.FileReader


import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.httpx.{Response, Http, Request}
import com.twitter.finagle.loadbalancer.HeapBalancerFactory
import com.twitter.util.Future
import com.typesafe.config.ConfigFactory

import scala.collection.JavaConversions._
import scalaz.{-\/, \/, \/-}

package object borderpatrol {

  implicit class AnyOps[A](val any: A) extends AnyVal {
    def toFuture: Future[A] =
      Future.value[A](any)
  }

  /**
   * Build upstream clients from borderpatrol.conf. A map of the clients (where service name is the key)
   * gets passed to the UpstreamService, which dispatches requests based on the service name
   * @return
   */
  def getUpstreamClients: Map[String, Service[Request, Response]] = {

    val conf = ConfigFactory.parseReader(new FileReader("borderpatrol.conf"))
    val services = conf.getConfigList("services").toList
    case class ServiceConfiguration(name: String, friendlyName: String, hosts: String, rewriteRule: String) {}

    val clients = services map (s =>
      (s.getString("name"),
        ClientBuilder()
          .codec(Http())
          .hosts(s.getString("hosts"))
          .hostConnectionLimit(10)
          .loadBalancer(HeapBalancerFactory.toWeighted)
          .retries(2)
          .build()))
    clients.toMap
  }

  trait Serializable[A] {
    def asJson(a: A): Option[String]
    def asBytes(a: A): Option[Array[Byte]]

    case class SerializedResult[Type](result: String \/ Type) {
      def isError: Boolean =
        result.isLeft
      def isSuccess: Boolean =
        result.isRight
      def toOption: Option[Type] =
        result.toOption
      def value: Option[Type] =
        result.toOption
      def failure: Option[String] =
        result.swap.toOption
      def map[B](f: Type => B): SerializedResult[B] =
        SerializedResult(result map f)
      def flatMap[B](f: Type => SerializedResult[B]): SerializedResult[B] =
        SerializedResult(result flatMap(f(_).result))
      override def toString(): String = s"SerializedResult($result)"
    }

    object SerializedResult {
      def ok[B](value: B): SerializedResult[B] = SerializedResult(\/-(value))
      def fail[B](s: String): SerializedResult[B] = SerializedResult(-\/(s))
    }
  }

  object Serializable {
    import session.SessionCodecJson
    import session.SessionOps

    implicit object SerializedSession extends Serializable[Session] {
      def asJson(session: Session): Option[String] =
        SessionCodecJson.encode(session).string
      def asBytes(session: Session): Option[Array[Byte]] =
        Some(session.asBytes)
    }
  }

}
