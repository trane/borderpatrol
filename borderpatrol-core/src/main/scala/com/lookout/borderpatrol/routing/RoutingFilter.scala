package com.lookout.borderpatrol.routing


import java.io.Serializable

import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Response
import com.twitter.finagle.Service
import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.SimpleFilter
import scala.collection.immutable.HashMap
import org.jboss.netty.handler.codec.http._
import scala.util.matching.Regex

class RoutingFilter
  extends SimpleFilter[HttpRequest, HttpResponse] {

  // Ideally the route would map a string to a new object with
  // details like service name and host information etc. For demo purposes we are going for
  // a string to string mapping of service names. "b" => "smb", "c" => "flexd" but we are hardcoding it
  // to SERVICE_NAME for simplicity
  //
  val routes = HashMap[String, HashMap[String, String]](
    "/b"->HashMap[String,String]("service_name"->"smb","host"->"localhost","port"->"9292"),
    "/a"->HashMap[String,String]("service_name"->"checkpoint","host"->"localhost","port"->"4567"),
    "default"->HashMap[String,String]("service_name"->"flexd","host"->"localhost","port"->"8080"))

  val pattern = new Regex("^/([^/]+)")

  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    // Ideally we would have a RoutingService looking up possible routes and not
    // have a static lookup table.
    val req = injectHeaderAndModifyUri(request)
    service(req)
  }


  def extractServiceFromUri(uri: String): Serializable = {
    val result = pattern.findFirstIn(uri).getOrElse("default")
    val serviceUri = {
      routes.getOrElse(result , "")
    }
    serviceUri
  }

  def injectHeaderAndModifyUri(request: HttpRequest): HttpRequest = {
    println("INITIAL HEADERS : "+ request.getHeaders)
    val service = extractServiceFromUri(request.getUri).asInstanceOf[HashMap[String,String]]
    request.addHeader("SERVICE_NAME", service.get("service_name").getOrElse("SERVICE_NAME"))
    request.addHeader("ORIGINAL_HOST", request.getHeader("HOST"))
    request.setHeader("HOST", service.get("host").getOrElse("localhost")+":"+service.get("port").getOrElse("8080"))
    request.setUri(request.getUri)
    println("HEADERS AFTER REROUTE : "+ request.getHeaders)
    request
  }

}

