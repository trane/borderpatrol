package com.lookout.borderpatrol

import com.twitter.finagle.{Httpx, Service}
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.util.Future
import scala.collection.mutable

object Binder {

  /**
   * trait for the context binding
   * @tparam A
   */
  trait BinderContext[A] {
    def name(manager: A): String
    def hosts(manager: A): String
  }

  /**
   * Bind request pod
   */
  case class BindRequest[A](context: A, req: Request)

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
      cache.getOrElse(implicitly[BinderContext[A]].name(req.context),
        util.Combinators.tap(Httpx.newService(implicitly[BinderContext[A]].hosts(req.context)))(cli =>
          cache(implicitly[BinderContext[A]].name(req.context)) = cli)
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
  case class LoginManagerBinder() extends MBinder[LoginManager]
  case class ManagerBinder() extends MBinder[Manager]
  case class ServiceIdentifierBinder() extends MBinder[ServiceIdentifier]
}