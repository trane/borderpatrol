package com.lookout.borderpatrol

import java.net.URL
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.{Status, Response, Request}
import com.twitter.finagle.httpx.path.Path
import com.twitter.util.Future

case class Manager(name: String, path: Path, hosts: Set[URL])

case class LoginManager(name: String, identityManager: Manager, accessManager: Manager,
                        protoManager: ProtoManager)

trait ProtoManager {
  val loginConfirm: Path
  def redirectLocation(host: String): String
  def hosts: Set[URL]
  def isMatchingPath(p: Path): Boolean
  def getOwnedPaths: Set[Path]
}

case class InternalProtoManager(loginConfirm: Path, path: Path, hsts: Set[URL])
    extends ProtoManager {
  def redirectLocation(host: String): String = path.toString
  def hosts: Set[URL] = hsts
  def isMatchingPath(p: Path): Boolean =
    !Set(path, loginConfirm).filter(p.startsWith(_)).isEmpty
  def getOwnedPaths: Set[Path] = Set(path)
}

case class OAuth2CodeProtoManager(loginConfirm: Path, authorizeUrl: URL, tokenUrl: URL,
                                  clientId: String, clientSecret: String)
    extends ProtoManager{
  def redirectLocation(host: String): String =
    Request.queryString(authorizeUrl.toString, ("response_type", "code"), ("state", "foo"),
      ("client_id", clientId), ("redirect_uri", "http://" + host + loginConfirm.toString))
  def hosts: Set[URL] = Set(authorizeUrl)
  def isMatchingPath(p: Path): Boolean = p.startsWith(loginConfirm)
  def getOwnedPaths: Set[Path] = Set.empty
}
