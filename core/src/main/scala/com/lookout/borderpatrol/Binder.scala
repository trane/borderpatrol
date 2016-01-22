package com.lookout.borderpatrol

import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.logging.Logger

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Response, Request}
import com.twitter.util.Future
import scala.collection.JavaConverters._
import scala.language.postfixOps


/**
 * Binder object defines methods and shells used to bind to upstream endpoints
 */
object Binder {

  /**
   * The trait for the context binding. It exposes common methods
   * to be made available from all the contexts.
   * @tparam A
   */
  trait BinderContext[A] {
    def name(manager: A): String
    def hosts(manager: A): Set[URL]
  }

  /**
   * Pod that contains the context (which we are trying to bind) and
   * the actual request to be processed and to be sent to endpoint
   */
  case class BindRequest[A: BinderContext](context: A, req: Request) {
    def name: String = implicitly[BinderContext[A]].name(context)
    def hosts: Set[URL] = implicitly[BinderContext[A]].hosts(context)
  }

  /**
   * It enables dynamic binding to the endpoints (e.g. login service, identity service, etc)
   *
   * Note that BinderContext makes it possible to templatize this code for all the LoginManagerBinder, ManagerBinder,
   * ServiceIdentifierBinder, etc, by making calls to methods (e.g. name & hosts) visible in the template.
   */
  abstract class MBinder[A: BinderContext]
      extends Service[BindRequest[A], Response] {
    def apply(req: BindRequest[A]): Future[Response] = BinderBase.connect(req.name, req.hosts, req.req)
  }

  /**
   * implicit values for evidence parameter of type BinderContext
   */
  implicit object LoginManagerBinderContext extends BinderContext[LoginManager] {
    def name(lm: LoginManager): String = lm.name
    def hosts(lm: LoginManager): Set[URL] = lm.protoManager.hosts
  }
  implicit object ManagerBinderContext extends BinderContext[Manager] {
    def name(m: Manager): String = m.name
    def hosts(m: Manager): Set[URL] = m.hosts
  }
  implicit object ServiceIdentifierBinderContext extends BinderContext[ServiceIdentifier] {
    def name(sid: ServiceIdentifier): String = sid.name
    def hosts(sid: ServiceIdentifier): Set[URL] = sid.hosts
  }

  /**
   * Binder objects
   */
  case object LoginManagerBinder extends MBinder[LoginManager]
  case object ManagerBinder extends MBinder[Manager]
  case object ServiceIdentifierBinder extends MBinder[ServiceIdentifier]
}


object BinderBase {
  val log = Logger.getLogger(getClass.getSimpleName)
  val cache: collection.concurrent.Map[String, Service[Request, Response]] =
    new ConcurrentHashMap[String, Service[Request, Response]] asScala

  private[this] def client(name: String, urls: Set[URL]): Service[Request, Response] = {
    // If its https, use TLS
    val https = urls.filter(u => u.getProtocol == "https").nonEmpty
    val hostname = urls.map(u => u.getHost).mkString

    // Find CSV of host & ports
    val hostAndPorts = urls.map(u => u.getAuthority).mkString(",")

    // Create
    if (https) Http.client.withTls(hostname).newService(hostAndPorts)
    else Http.newService(hostAndPorts)
  }

  private[this] def getOrCreate(name: String, urls: Set[URL]): Future[Service[Request, Response]] =
    cache.getOrElse(name, {
      //  Allocate a new client
      val cl = client(name, urls)
      // putIfAbsent atomically inserts the client into the map,
      val maybeC = cache.putIfAbsent(name, cl)
      // if maybeC has a value, we got pre-empted => abandon our new allocated cl
      // and return the present one. Otherwise, return newly allocate cl.
      maybeC.getOrElse(cl)
    }).toFuture

  def connect(name: String, urls: Set[URL], request: Request): Future[Response] = {
    (for {
      cl <- getOrCreate(name, urls)
      res <- cl.apply(request)
    } yield res) handle {
      case e => {
        log.warning("Failed to connect " +
          s"for: $name to: ${urls.map(u => u.getAuthority).mkString(",")} with: ${e.getMessage}")
        throw CommunicationError(s"${name} with ${e.getMessage}")
      }
    }
  }

  def get(name: String): Option[Service[Request, Response]] = cache.get(name)

  def clear(): Unit = cache.clear()
}
