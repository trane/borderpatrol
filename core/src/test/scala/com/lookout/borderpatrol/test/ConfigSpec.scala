package com.lookout.borderpatrol.test

import com.lookout.borderpatrol._
import com.twitter.finagle.httpx.path.Path
import scala.reflect.io.File

class ConfigSpec extends BorderPatrolSuite {

  val validContents = """[{"name":"one","path": {"str" :"customer1"},"subdomain":"customer1","login":"/login"}]"""
  val tempValidFile = File.makeTemp("ServerConfigValid", ".tmp")
  tempValidFile.writeAll(validContents)

  val invalidContents = "this is an invalid JSON file"
  val tempInvalidFile = File.makeTemp("ServerConfigSpecInvalid", ".tmp")
  tempInvalidFile.writeAll(invalidContents)

  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", "/a/login")

  behavior of "ServerConfig"

  it should "succeed to build a valid ServerConfig" in {
    val serverConfig = ServerConfig(Config.defaultSecretStore, Config.defaultSessionStore, Set(one))
    serverConfig should not be null
  }

//  it should "raise a ConfigError exception when No Secret Store config raises an exception" in {
//     val
//
//    val twitterServer = new TestTwitterServer
//    twitterServer.main(args = Seq("--serviceIds.file", "foo.json").toArray)
//    assert(twitterServer.bootstrapSeq ===
//      Seq('Init, 'PreMain, 'Main, 'PostMain, 'Exit))
//    val caught = the [ConfigError] thrownBy {
//      ServerConfig(emptyServersList, false, emptyServersList, true,
//        Seq(new InetSocketAddress("localhost", 11211)),
//        tempValidFile.toCanonical.toString)
//    }
//    caught.getMessage should equal ("Invalid SecretStore")
//  }
//
//  it should "Invalid SessionStore raises an exception" in {
//    val caught = the [ConfigError] thrownBy {
//      ServerConfig(emptyServersList, true, emptyServersList, false,
//        Seq(new InetSocketAddress("localhost", 11211)),
//        tempValidFile.toCanonical.toString)
//    }
//    caught.getMessage should equal ("Invalid SessionStore")
//  }
//
//  it should "Invalid Filename for ServiceIdentifiers raises an exception" in {
//    val caught = the [java.io.FileNotFoundException] thrownBy {
//      ServerConfig(emptyServersList, true, emptyServersList, true,
//        Seq(new InetSocketAddress("localhost", 11211)), "badfilename")
//    }
//    caught.getMessage should equal ("badfilename (No such file or directory)")
//  }
//
//  it should "Failure to decode list of ServiceIdentifiers raises an exception" in {
//    val caught = the [ConfigError] thrownBy {
//      ServerConfig(emptyServersList, true, emptyServersList, true,
//        Seq(new InetSocketAddress("localhost", 11211)),
//        tempInvalidFile.toCanonical.toString)
//    }
//    caught.getMessage should equal ("ParsingFailure: expected true got t (line 1, column 1)")
//  }
}
