package com.lookout.borderpatrol.server

import java.net.URL

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol._
import com.lookout.borderpatrol.test.{sessionx, BorderPatrolSuite}
import com.twitter.finagle.httpx.path.Path

class ServicesSpec extends BorderPatrolSuite {
  import sessionx.helpers._
  import services._

  val urls = Set(new URL("http://localhost:5678"))

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val internalProtoManager = InternalAuthProtoManager(Path("/loginConfirm"), Path("/check"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", keymasterIdManager, keymasterAccessManager,
    internalProtoManager)

  // sids
  val one = ServiceIdentifier("one", urls, Path("/ent"), None)
  val cid = CustomerIdentifier("enterprise", one, checkpointLoginManager)
  val sids = Set(one)
  val cids = Set(cid)
  val serviceMatcher = ServiceMatcher(cids, sids)

  //  Config helpers
  val defaultStatsdExporterConfig = StatsdExporterConfig("host", 300, "prefix")
  val defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val defaultSessionStore = SessionStores.InMemoryStore
  val serverConfig = ServerConfig(defaultSecretStore, defaultSessionStore, defaultStatsdExporterConfig,
    cids, sids, Set(checkpointLoginManager), Set(keymasterIdManager), Set(keymasterAccessManager))

  /***FIXME: use this write integration test-suite */
  behavior of "MainServiceChain"

  it should "construct a valid service chain" in {
    implicit val conf = serverConfig
    MainServiceChain should not be null
  }
}