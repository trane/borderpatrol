package com.lookout.borderpatrol.test

import org.scalatest._

/**
 * A trait to provide consistency and reduce boilerplate for testing in Border Patrol
 */
trait BorderPatrolSuite extends FlatSpec with Matchers with TryValues with OptionValues with BeforeAndAfterEach
