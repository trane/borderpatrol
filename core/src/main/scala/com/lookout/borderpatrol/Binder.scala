package com.lookout.borderpatrol

import com.twitter.finagle.{Httpx, Service}
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.util.Future
import scala.collection.mutable

object Binder {

  /**
   * The trait for the context binding. It exposes common methods to be made available from all the contexts.
   *
   * @tparam A
   */
  trait BinderContext[A] {
    def name(manager: A): String
    def hosts(manager: A): String
  }

  /**
   * Pod that contains the context (which we are trying to bind) and
   * the actual request to be processed and to be sent to endpoint
   */
  case class BindRequest[A: BinderContext](context: A, req: Request) {
    def name: String = implicitly[BinderContext[A]].name(context)
    def hosts: String = implicitly[BinderContext[A]].hosts(context)
  }

  /**
   * It enables dynamic binding to the endpoints (e.g. login service, identity service, etc)
   *
   * Note that BinderContext makes it possible to templatize this code for all the LoginManagerBinder, ManagerBinder,
   * ServiceIdentigfierBinder, etc, by making calls to methods (e.g. name & hosts) visible in the template.
   *
   * @param cache Caches the already established client service
   * @tparam A
   */
  abstract class MBinder[A: BinderContext](cache: mutable.Map[String, Service[Request, Response]] =
                                       mutable.Map.empty[String, Service[Request, Response]])
    extends Service[BindRequest[A], Response] {
    def apply(req: BindRequest[A]): Future[Response] = {
      cache.getOrElse(req.name,
        util.Combinators.tap(Httpx.newService(req.hosts))(cli => cache(req.name) = cli)
      ).apply(req.req)
    }
    def get(name: String): Option[Service[Request, Response]] = cache.get(name)
  }

  /**
   * implicit values for evidence parameter of type BinderContext
   */
  implicit object LoginManagerBinderContext extends BinderContext[LoginManager] {
    def name(lm: LoginManager): String = lm.name
    def hosts(lm: LoginManager): String = lm.hosts
  }
  implicit object ManagerBinderContext extends BinderContext[Manager] {
    def name(m: Manager): String = m.name
    def hosts(m: Manager): String = m.hosts
  }
  implicit object ServiceIdentifierBinderContext extends BinderContext[ServiceIdentifier] {
    def name(sid: ServiceIdentifier): String = sid.name
    def hosts(sid: ServiceIdentifier): String = sid.hosts
  }

  /**
   * Binder objects
   */
  case object LoginManagerBinder extends MBinder[LoginManager]
  case object ManagerBinder extends MBinder[Manager]
  case object ServiceIdentifierBinder extends MBinder[ServiceIdentifier]
}
