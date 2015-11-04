package com.lookout.borderpatrol

import com.twitter.finagle.httpx.path.Path

/**
 *
 * @param name
 * @param path
 * @param hosts
 */
case class Manager(name: String, path: Path, hosts: String)

/**
 *
 * @param name
 * @param path
 * @param hosts
 * @param loginPath
 * @param identityManager
 * @param accessManager
 */
case class LoginManager(name: String, path: Path, hosts: String,
                        loginPath: Path, identityManager: Manager, accessManager: Manager)
