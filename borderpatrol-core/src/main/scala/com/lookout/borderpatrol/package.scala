package com.lookout

import java.io.FileReader


import argonaut.Json
import com.twitter.bijection.{Bijection, Base64String, Injection}
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
    def toFailedFuture(e: Throwable): Future[A] =
      Future.exception(e)
  }

  lazy val bytes2b64str = Injection.connect[Array[Byte], Base64String, String]
  lazy val json2bytes = Injection.connect[Json, String, Array[Byte]]
  lazy val seq2IndexedSeq = Bijection.seq2IndexedSeq

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
    def as[B](a: A)(implicit f: A => Option[B]): Option[B]

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

  trait View[A,B] {
    def apply(a: A): B
  }

  object View {
    def apply[A,B](f: A => B): View[A,B] = new View[A,B] {
      def apply(a: A): B =
        f(a)
      implicit def identity[A]: View[A,A] = View(a => a)
    }
  }

  type %>[A, B] = View[A, B]

}
