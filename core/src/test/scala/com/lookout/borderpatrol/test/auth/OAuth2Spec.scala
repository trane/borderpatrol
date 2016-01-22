package com.lookout.borderpatrol.auth

import java.math.BigInteger
import java.security.{PublicKey, KeyPairGenerator, KeyPair}
import java.security.interfaces.{ECPublicKey, ECPrivateKey, RSAPublicKey, RSAPrivateKey}
import java.util.Date
import javax.security.auth.x500.X500Principal
import javax.xml.bind.DatatypeConverter

import com.lookout.borderpatrol.{BinderBase, CommunicationError}
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test.{sessionx, BorderPatrolSuite}
import com.lookout.borderpatrol.util.Combinators.tap
import com.nimbusds.jose.{JWSVerifier, JWSSigner, JWSHeader, JWSAlgorithm}
import com.nimbusds.jose.crypto.{ECDSAVerifier, ECDSASigner, RSASSAVerifier, RSASSASigner}
import com.nimbusds.jose.util.{X509CertUtils, Base64URL}
import com.nimbusds.jwt.{PlainJWT, SignedJWT, JWTClaimsSet}
import com.twitter.finagle.http._
import com.twitter.finagle.http.service.RoutingService
import com.twitter.util.Await
import org.bouncycastle.x509.X509V1CertificateGenerator


class OAuth2Spec extends BorderPatrolSuite {
  import sessionx.helpers._
  import OAuth2._

  override def afterEach(): Unit = {
    BinderBase.clear
  }

  // Mock
  class MockOAuth2CodeVerify extends OAuth2CodeVerify {
    def mockAdd(name: String, certificate: String): Unit = add(name, certificate)

    def mockVerifier(pk: PublicKey): JWSVerifier = verifier(pk)
  }

  // Simple AAD tokens
  val simpleAadToken = AadToken("TestAccessToken", "TestIdToken")
  val simpleEmptyAadToken = AadToken("", "")

  // RSA signatures require a public and private RSA key pair, the public key
  // must be made known to the JWS recipient in order to verify the signatures
  def generateTestKeyPair(algorithm: String, keySize: Int) = {
    val keyGenerator: KeyPairGenerator = KeyPairGenerator.getInstance(algorithm)
    keyGenerator.initialize(keySize)
    keyGenerator.genKeyPair()
  }

  def generateTestCertificate(kp: KeyPair, algorithm: String) = {
    val startDate: Date = new Date(2010, 1, 1)
    // time from which certificate is valid
    val expiryDate: Date = new Date(2020, 1, 1)
    // time after which certificate is not valid
    val serialNumber: BigInteger = BigInteger.TEN
    // serial number for certificate
    val certGen: X509V1CertificateGenerator = new X509V1CertificateGenerator()
    val dnName: X500Principal = new X500Principal("CN=Test CA Certificate")
    certGen.setSerialNumber(serialNumber)
    certGen.setIssuerDN(dnName)
    certGen.setNotBefore(startDate)
    certGen.setNotAfter(expiryDate)
    certGen.setSubjectDN(dnName) // note: same as issuer
    certGen.setPublicKey(kp.getPublic())
    certGen.setSignatureAlgorithm(algorithm)
    certGen.generate(kp.getPrivate())
  }

  // RSA Stuff
  val testRsaKeyPair = generateTestKeyPair("RSA", 1024)
  val testRsaSigner: JWSSigner = new RSASSASigner(testRsaKeyPair.getPrivate.asInstanceOf[RSAPrivateKey])
  val testRsaCertificate = generateTestCertificate(testRsaKeyPair, "SHA256withRSA")
  val testRsaCertificateEncoded = DatatypeConverter.printBase64Binary(testRsaCertificate.getEncoded)
  val testRsaCertificateThumb = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val dec = DatatypeConverter.parseBase64Binary(testRsaCertificateEncoded)
    DatatypeConverter.printBase64Binary(md.digest(dec)).replaceAll("=", "").replaceAll("/", "_")
  }
  val testRsaAccessToken = new SignedJWT(
    new JWSHeader.Builder(JWSAlgorithm.RS256).x509CertThumbprint(new Base64URL(testRsaCertificateThumb)).build,
    new JWTClaimsSet.Builder().subject("abc123").claim("upn", "test@example.com").build)
  testRsaAccessToken.sign(testRsaSigner)
  val testRsaIdToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomeIdToken")
    .claim("upn", "test@example.com").build)
  val testRsaAadToken = AadToken(testRsaAccessToken.serialize(), testRsaIdToken.serialize())

  // EC Stuff
  val testEcKeyPair = generateTestKeyPair("EC", 256)
  val testEcSigner: JWSSigner = new ECDSASigner(testEcKeyPair.getPrivate.asInstanceOf[ECPrivateKey])
  val testEcCertificate = generateTestCertificate(testEcKeyPair, "SHA256withECDSA")
  val testEcCertificateEncoded = DatatypeConverter.printBase64Binary(testEcCertificate.getEncoded)
  val testEcCertificateThumb = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val dec = DatatypeConverter.parseBase64Binary(testEcCertificateEncoded)
    DatatypeConverter.printBase64Binary(md.digest(dec)).replaceAll("=", "").replaceAll("/", "_")
  }
  val testEcAccessToken = new SignedJWT(
    new JWSHeader.Builder(JWSAlgorithm.ES256).x509CertThumbprint(new Base64URL(testEcCertificateThumb)).build,
    new JWTClaimsSet.Builder().subject("abc123").claim("upn", "test@example.com").build)
  testEcAccessToken.sign(testEcSigner)
  val testEcIdToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomeIdToken")
    .claim("upn", "test@example.com").build)
  val testEcAadToken = AadToken(testEcAccessToken.serialize(), testEcIdToken.serialize())
  
  behavior of "AadToken"

  it should "uphold encoding/decoding AadToken" in {
    def encodeDecode(toks: AadToken): AadToken = {
      val encoded = AadTokenEncoder(toks)
      AadTokenDecoder.decodeJson(encoded).fold[AadToken](e => simpleEmptyAadToken, t => t)
    }
    encodeDecode(simpleAadToken) should be(simpleAadToken)
  }

  behavior of "codeToClaimsSet"

  it should "generate RSA Certificate" in {
    val certAgain = X509CertUtils.parse(DatatypeConverter.parseBase64Binary(testRsaCertificateEncoded))
    val certAgainStr = DatatypeConverter.printBase64Binary(certAgain.getEncoded)
    val againKey = certAgain.getPublicKey.asInstanceOf[RSAPublicKey]
    val againVerifier = new RSASSAVerifier(againKey)
    testRsaAccessToken.verify(againVerifier) should be (true)
  }

  it should "generate EC Certificate" in {
    val certAgain = X509CertUtils.parse(DatatypeConverter.parseBase64Binary(testEcCertificateEncoded))
    val certAgainStr = DatatypeConverter.printBase64Binary(certAgain.getEncoded)
    val againKey = certAgain.getPublicKey.asInstanceOf[ECPublicKey]
    val againVerifier = new ECDSAVerifier(againKey)
    testEcAccessToken.verify(againVerifier) should be (true)
  }

  it should "succeed and transform the Request with OAuth2 code to JWT ClaimsSet using RSA certificates" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testRsaAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
        case p2 if p2 contains "certificateUrl" => mkTestService[Request, Response] { req =>
          tap(Response(Status.Ok))(res => {
            res.contentString = s"""<EntityDescriptor xmlns="urn:oasis:names:tc:SAML:2.0:metadata" ID="_535c5971-00df-43d4-915e-841cfed13adc" entityID="https://sts.windows.net/{tenantid}/">
                <RoleDescriptor xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:fed="http://docs.oasis-open.org/wsfed/federation/200706" xsi:type="fed:SecurityTokenServiceType" protocolSupportEnumeration="http://docs.oasis-open.org/wsfed/federation/200706">
                <KeyDescriptor use="signing">
                <KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#">
                <X509Data>
                <X509Certificate>$testRsaCertificateEncoded</X509Certificate>
                </X509Data>
                </KeyInfo>
                </KeyDescriptor>
                </RoleDescriptor>
                </EntityDescriptor>"""
            res.contentType = "text/xml"
          }).toFuture
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      Await.result(output).getSubject should be("abc123")
      Await.result(output).getStringClaim("upn") should be("test@example.com")
    } finally {
      server.close()
    }
  }

  it should "succeed and transform the Request with OAuth2 code to JWT ClaimsSet using EC certificates" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testEcAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
        case p2 if p2 contains "certificateUrl" => mkTestService[Request, Response] { req =>
          tap(Response(Status.Ok))(res => {
            res.contentString = s"""<EntityDescriptor xmlns="urn:oasis:names:tc:SAML:2.0:metadata" ID="_535c5971-00df-43d4-915e-841cfed13adc" entityID="https://sts.windows.net/{tenantid}/">
                <RoleDescriptor xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:fed="http://docs.oasis-open.org/wsfed/federation/200706" xsi:type="fed:SecurityTokenServiceType" protocolSupportEnumeration="http://docs.oasis-open.org/wsfed/federation/200706">
                <KeyDescriptor use="signing">
                <KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#">
                <X509Data>
                <X509Certificate>$testEcCertificateEncoded</X509Certificate>
                </X509Data>
                </KeyInfo>
                </KeyDescriptor>
                </RoleDescriptor>
                </EntityDescriptor>"""
            res.contentType = "text/xml"
          }).toFuture
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      Await.result(output).getSubject should be("abc123")
      Await.result(output).getStringClaim("upn") should be("test@example.com")
    } finally {
      server.close()
    }
  }

  it should "succeed and use cached certificate to transform the Request with OAuth2 code to JWT ClaimsSet" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testRsaAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
        case p2 if p2 contains "certificateUrl" => mkTestService[Request, Response] { req =>
          fail("Use cached certificate, should not get here")
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Add certificate
      val oAuth2CodeVerify = new MockOAuth2CodeVerify()
      oAuth2CodeVerify.mockAdd(testRsaCertificateThumb, testRsaCertificateEncoded)

      // Execute
      val output = oAuth2CodeVerify.codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      Await.result(output).getSubject should be("abc123")
      Await.result(output).getStringClaim("upn") should be("test@example.com")
    } finally {
      server.close()
    }
  }

  it should "throw a BpCommunicationError if it fails to reach OAuth2 IDP to convert code to token" in {

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val loginRequest = req("rainy", "/signblew", ("code" -> "XYZ123"))

    // SessionIdRequest
    val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust3, three), sessionId)

    // Execute
    val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeBadProtoManager)

    // Validate
    val caught = the[CommunicationError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should startWith("An error occurred while talking to: " +
      "http://localhost:9999/tokenUrl with java.net.ConnectException: Connection refused:")
  }

  /** this exception is thrown by codeToToken method in OAuth2CodeProtoManager */
  it should "throw an Exception on if it receives HTTP request w/ OAuth2 code but without hostname" in {
    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val loginRequest = Request("/signin", ("code" -> "XYZ123"))

    // SessionIdRequest
    val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

    // Validate
    val caught = the[Exception] thrownBy {

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)
    }
    caught.getMessage should be("Host not found in HTTP Request")
  }

  it should "throw an exception if fails to parse OAuth2 AAD Token in the response" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = """{"key":"value"}"""
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[IdentityProviderError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should be("Failed to parse the AadToken received from OAuth2 Server: ulm")
      caught.status should be(Status.InternalServerError)
    } finally {
      server.close()
    }
  }

  it should "throw an exception if OAuth2 Server returns an failure response for code to token conversion" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        Response(Status.NotAcceptable).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[IdentityProviderError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should be("Failed to receive the AadToken from OAuth2 Server: ulm")
      caught.status should be(Status.NotAcceptable)
    } finally {
      server.close()
    }
  }

  it should "throw an Exception if it fails to parse ID-token in AAD-token returned by OAuth2 Server" in {
    val idToken = "stuff" //"""{"key":"value"}"""
    val aadToken = AadToken(testRsaAccessToken.serialize(), idToken)
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpTokenParsingError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to parse token with: Invalid serialized")
    } finally {
      server.close()
    }
  }

  it should "throw an Exception if it fails to parse Access-token in AAD-token returned by OAuth2 Server" in {
    val accessToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomethingAccess").build)
    val aadToken = AadToken(accessToken.serialize(), testRsaIdToken.serialize())
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpTokenParsingError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to parse token with: Invalid ")
    } finally {
      server.close()
    }
  }

  it should "throw an Exception if it fails to find thumbprint in the Access-token returned by OAuth2 Server" in {
    // Prepare JWT with claims set
    val accessToken = new SignedJWT(
      new JWSHeader.Builder(JWSAlgorithm.RS256).build,
      new JWTClaimsSet.Builder().subject("abc123").claim("upn", "test@example.com").build)
    accessToken.sign(testRsaSigner)
    val aadToken = AadToken(accessToken.serialize(), testRsaIdToken.serialize())
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpTokenParsingError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to parse token with: Wrapping null input")
    } finally {
      server.close()
    }
  }

  it should "throw an exception if it fails to find Certificate in the XML code returned by OAuth2 Server" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testRsaAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
        case p2 if p2 contains "certificateUrl" => mkTestService[Request, Response] { req =>
          tap(Response(Status.Ok))(res => {
            res.contentString = s"""<EntityDescriptor xmlns="urn:oasis:names:tc:SAML:2.0:metadata" ID="_535c5971-00df-43d4-915e-841cfed13adc" entityID="https://sts.windows.net/{tenantid}/">
                <RoleDescriptor xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:fed="http://docs.oasis-open.org/wsfed/federation/200706" xsi:type="fed:SecurityTokenServiceType" protocolSupportEnumeration="http://docs.oasis-open.org/wsfed/federation/200706">
                <KeyDescriptor use="signing">
                <KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#">
                <X509Data>
                </X509Data>
                </KeyInfo>
                </KeyDescriptor>
                </RoleDescriptor>
                </EntityDescriptor>"""
            res.contentType = "text/xml"
          }).toFuture
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpCertificateError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to process Certificate with: Unable to find certificate for")
    } finally {
      server.close()
    }
  }

  it should "throw an exception if it fails to parse XML code returned by OAuth2 Server" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testRsaAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
        case p2 if p2 contains "certificateUrl" => mkTestService[Request, Response] { req =>
          tap(Response(Status.Ok))(res => {
            res.contentString = s"""<EntityDescriptor xmlns="urn:oasis:names:tc:SAML:2.0:metadata" ID="_535c5971-00df-43d4-915e-841cfed13adc" entityID="https://sts.windows.net/{tenantid}/">
                <RoleDescriptor xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:fed="http://docs.oasis-open.org/wsfed/federation/200706" xsi:type="fed:SecurityTokenServiceType" protocolSupportEnumeration="http://docs.oasis-open.org/wsfed/federation/200706">
                <KeyDescriptor use="signing">
                <KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#">
                <X509Data>
                <X509Certificate>$testRsaCertificateEncoded</X509Certificate>
                </X509Data>
                </KeyInfo>
                </EntityDescriptor>"""
            res.contentType = "text/xml"
          }).toFuture
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Execute
      val output = new OAuth2CodeVerify().codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpCertificateError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to process Certificate with: The element type")
    } finally {
      server.close()
    }
  }

  it should "throw an exception if it fails to decode Certificate returned by OAuth2 Server" in {

    //Launch a server for fetching token from a code
    val server = com.twitter.finagle.Http.serve(
      "localhost:4567",
      RoutingService.byPath {
        case p1 if p1 contains "tokenUrl" => mkTestService[Request, Response] { req =>
          assert(req.getParam("code") == "XYZ123")
          tap(Response(Status.Ok))(res => {
            res.contentString = AadTokenEncoder(testRsaAadToken).toString()
            res.contentType = "application/json"
          }).toFuture
        }
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // SessionIdRequest
      val sessionIdRequest = SessionIdRequest(ServiceRequest(loginRequest, cust2, two), sessionId)

      // Add certificate
      val oAuth2CodeVerify = new MockOAuth2CodeVerify()
      oAuth2CodeVerify.mockAdd(testRsaCertificateThumb, "SOMEHACKCERTIFICATE")

      // Execute
      val output = oAuth2CodeVerify.codeToClaimsSet(sessionIdRequest, oauth2CodeProtoManager)

      // Validate
      val caught = the[BpCertificateError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith("Failed to process Certificate with: Wrapping null input argument")
    } finally {
      server.close()
    }
  }

  it should "throw an exception if algorithm of public key used by the certificate is unsupported " in {

    //  Generate key
    val pk = generateTestKeyPair("DSA", 1024)

    // Validate
    val caught = the[BpCertificateError] thrownBy {
      val output = new MockOAuth2CodeVerify().mockVerifier(pk.getPublic)
    }
    caught.getMessage should startWith("Failed to process Certificate with: Unsupported PublicKey algorithm")
  }
}
