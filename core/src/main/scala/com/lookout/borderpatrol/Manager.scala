package com.lookout.borderpatrol

import java.net.URL
import com.twitter.finagle.http.{Method, Response, Request}
import com.twitter.finagle.http.path.Path
import com.twitter.util.Future

case class Manager(name: String, path: Path, hosts: Set[URL])

case class LoginManager(name: String, identityManager: Manager, accessManager: Manager,
                        protoManager: ProtoManager)

trait ProtoManager {
  val loginConfirm: Path
  def redirectLocation(host: Option[String]): String
  def hosts: Set[URL]
  def isMatchingPath(p: Path): Boolean
  def getOwnedPaths: Set[Path]
}

case class InternalAuthProtoManager(loginConfirm: Path, path: Path, hsts: Set[URL])
    extends ProtoManager {
  def redirectLocation(host: Option[String]): String = path.toString
  def hosts: Set[URL] = hsts
  def isMatchingPath(p: Path): Boolean = Set(path, loginConfirm, Path("/logout")).filter(p.startsWith(_)).nonEmpty
  def getOwnedPaths: Set[Path] = Set(path)
}

case class OAuth2CodeProtoManager(loginConfirm: Path, authorizeUrl: URL, tokenUrl: URL, certificateUrl: URL,
                                  clientId: String, clientSecret: String)
    extends ProtoManager{
  def redirectLocation(host: Option[String]): String = {
    val hostStr = host.getOrElse(throw new Exception("Host not found in HTTP Request"))
    Request.queryString(authorizeUrl.toString, ("response_type", "code"), ("state", "foo"),
      ("client_id", clientId), ("redirect_uri", "http://" + hostStr + loginConfirm.toString))
  }
  def hosts: Set[URL] = Set(authorizeUrl)
  def isMatchingPath(p: Path): Boolean = Set(loginConfirm, Path("/logout")).filter(p.startsWith(_)).nonEmpty
  def getOwnedPaths: Set[Path] = Set.empty
  def codeToToken(host: Option[String], code: String): Future[Response] = {
    val hostStr = host.getOrElse(throw new Exception("Host not found in HTTP Request"))
    val request = util.Combinators.tap(Request(Method.Post, tokenUrl.toString))(re => {
      re.contentType = "application/x-www-form-urlencoded"
      re.contentString = Request.queryString(("grant_type", "authorization_code"), ("client_id", clientId),
        ("code", code), ("redirect_uri", "http://" + hostStr + loginConfirm.toString),
        ("client_secret", clientSecret), ("resource", "00000002-0000-0000-c000-000000000000"))
        .drop(1) /* Drop '?' */
    })
    BinderBase.connect(tokenUrl.toString, Set(tokenUrl), request)
  }
}
