package com.lookout.borderpatrol

import java.net.URL
import com.twitter.finagle.httpx.path.Path

/**
 *
 * @param name
 * @param path
 * @param hosts List of URLs to upstream manager
 */
case class Manager(name: String, path: Path, hosts: Set[URL])

/**
 *
 * @param name
 * @param path
 * @param hosts list of URLs to upstream manager
 * @param loginPath
 * @param identityManager
 * @param accessManager
 */
case class LoginManager(name: String, path: Path, hosts: Set[URL],
                        loginPath: Path, identityManager: Manager, accessManager: Manager)
