package com.lookout.borderpatrol.auth

import java.net.URL
import java.security.PublicKey
import java.security.interfaces.{ECPublicKey, RSAPublicKey}
import java.util.logging.Logger
import javax.xml.bind.DatatypeConverter

import com.twitter.logging.Level
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Encoder
import com.lookout.borderpatrol.{OAuth2CodeProtoManager, BinderBase}
import com.lookout.borderpatrol.sessionx._
import com.nimbusds.jose.JWSVerifier
import com.nimbusds.jose.crypto.{ECDSAVerifier, RSASSAVerifier}
import com.nimbusds.jose.util.X509CertUtils
import com.nimbusds.jwt.{PlainJWT, SignedJWT, JWTClaimsSet}
import com.twitter.finagle.http.{Status, Response, Request}
import com.twitter.util.Future

import scala.util.{Failure, Success, Try}
import scala.xml.{NodeSeq, Elem}

object OAuth2 {

  import io.circe.generic.semiauto._
  import cats.data.Xor

  def derive[A : Decoder](input: String): Xor[Error, A] =
    jawn.decode[A](input)

  /**
   * AAD token
   * @param accessToken JWT Access Token
   * @param idToken JWT ID Token
   */
  case class AadToken(accessToken: String, idToken: String)

  /**
   * AadToken Encoder/Decoder
   */
  implicit val AadTokenEncoder: Encoder[AadToken] = Encoder.instance {t =>
    Json.fromFields(Seq(
      ("access_token", t.accessToken.asJson),
      ("id_token", t.idToken.asJson)))
  }

  implicit val AadTokenDecoder: Decoder[AadToken] = Decoder.instance {c =>
    for {
      accessToken <- c.downField("access_token").as[String]
      idToken <- c.downField("id_token").as[String]
    } yield AadToken(accessToken, idToken)
  }

  /**
   * This class downloads and manages the certificates and verifies the tokens
   */
  class OAuth2CodeVerify {
    private[this] val log = Logger.getLogger(getClass.getSimpleName)
    private[this] var certificates: Map[String, String] = Map.empty[String, String]

    private[this] def find(name: String): Option[String] =
      certificates.get(name)

    protected[this] def add(name: String, certificate: String): Unit =
      certificates = (this.certificates + ((name, certificate)))

    private[this] def decodeCertFromXml(xml: Elem): NodeSeq = {
      (xml \\ "_" filter (node =>
        node.attributes.exists(_.value.text == "fed:SecurityTokenServiceType"))) \\ "X509Certificate"
    }

    private[this] def download_aad_certs(downloadUrl: URL, thumbprint: String): Future[String] = {
      //  Fetch the response
      BinderBase.connect("AADCert-" + downloadUrl.toString, Set(downloadUrl),
        Request(downloadUrl.getPath)).flatMap(res => res.status match {

        //  Parse for Tokens if Status.Ok
        case Status.Ok => {

          // Load xml
          val xml = Try(scala.xml.XML.loadString(res.contentString.replaceAll("[^\\x20-\\x7e]", ""))) match {
            case Success(v) => v
            case Failure(f) => throw new BpCertificateError(f.getMessage)
          }

          // Parse xml for certificate tags and then add all certificates to cache
          decodeCertFromXml(xml).foreach(node => {
            val md = java.security.MessageDigest.getInstance("SHA-1")
            val dec = DatatypeConverter.parseBase64Binary(node.text)
            val thumb = DatatypeConverter.printBase64Binary(md.digest(dec)).replaceAll("=", "").replaceAll("/", "_")
            // Add it to the cache
            add(thumb, node.text)
            log.log(Level.DEBUG, s"downloaded a certificate for thumbprint: " + thumb)
          })

          // Find again or throw exception
          find(thumbprint).getOrElse(throw new BpCertificateError(
            s"Unable to find certificate for thumbprint: $thumbprint")).toFuture
        }

        //  Preserve Response Status code by throwing AccessDenied exceptions
        case _ => Future.exception(BpCertificateError("Failed to download certificate from Server with: " +
          res.status))
      })
    }

    protected[this] def verifier(pk: PublicKey): JWSVerifier =
      pk match {
        case rsaPk: RSAPublicKey => new RSASSAVerifier(rsaPk)
        case ecPk: ECPublicKey => new ECDSAVerifier(ecPk)
        case _ => throw new BpCertificateError(s"Unsupported PublicKey algorithm: ${pk.getAlgorithm}")
      }

    /**
     * Parse the signed token, download the certificate/public key if necessary and verify the signature
     *
     * @param certificateUrl
     * @param tokenStr
     * @return
     */
    private[this] def getClaimsSet(certificateUrl: URL, tokenStr: String): Future[JWTClaimsSet] = {
      for {
        signedJWT <- wrapFuture({ () => SignedJWT.parse(tokenStr) }, BpTokenParsingError.apply)
        thumbprint <- wrapFuture({() => signedJWT.getHeader.getX509CertThumbprint }, BpTokenParsingError.apply)
        certStr <- find(thumbprint.toString).fold(download_aad_certs(
          certificateUrl, thumbprint.toString))(Future.value(_))
        cert <- wrapFuture({ () => X509CertUtils.parse(DatatypeConverter.parseBase64Binary(certStr)) },
          BpCertificateError.apply)
      } yield signedJWT.verify(verifier(cert.getPublicKey)) match {
        case true => {
          log.log(Level.DEBUG, "Verified the signature on the AccessToken for: " +
            s"${signedJWT.getJWTClaimsSet.getStringClaim("upn")}, with a certificate of thumbprint: " + thumbprint)
          signedJWT.getJWTClaimsSet
        }
        case false => {
          log.log(Level.DEBUG, "Failed to verified the signature on the AccessToken for: " +
            s"${signedJWT.getJWTClaimsSet.getStringClaim("upn")}, with a certificate of thumbprint: " + thumbprint)
          throw new Exception("failed to verify signature")
        }
      }
    }

    /**
     * Download the AAD tokens, have the Access Token verified and return it to callers
     *
     * @param req
     * @param protoManager
     * @return
     */
    def codeToClaimsSet(req: SessionIdRequest, protoManager: OAuth2CodeProtoManager): Future[JWTClaimsSet] = {
      for {
        aadToken <- protoManager.codeToToken(
          req.req.host, req.req.getParam("code")).flatMap(res => res.status match {
          //  Parse for Tokens if Status.Ok
          case Status.Ok =>
            OAuth2.derive[AadToken](res.contentString).fold[Future[AadToken]](
              err => Future.exception(IdentityProviderError(Status.InternalServerError,
                "Failed to parse the AadToken received from OAuth2 Server: " +
                  s"${req.customerId.loginManager.name}")),
              t => Future.value(t)
            )
          //  Preserve Response Status code by throwing AccessDenied exceptions
          case _ => Future.exception(IdentityProviderError(res.status,
            s"Failed to receive the AadToken from OAuth2 Server: ${req.customerId.loginManager.name}"))
        })
        idClaimSet <- wrapFuture({() => PlainJWT.parse(aadToken.idToken).getJWTClaimsSet}, BpTokenParsingError.apply)
        accessClaimSet <- getClaimsSet(protoManager.certificateUrl, aadToken.accessToken)
      } yield accessClaimSet
    }
  }
}
