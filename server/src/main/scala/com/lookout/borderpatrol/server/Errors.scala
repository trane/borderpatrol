package com.lookout.borderpatrol.server

// scalastyle:off null
case class ConfigError(message: String)
  extends Exception(s"An error occurred while reading BorderPatrol Configuration: ${message}", null)

case class DuplicateConfigError(key: String, field: String)
  extends Exception("An error occurred while reading BorderPatrol Configuration: " +
    s"Duplicate entries for key(s) (${key}) - are found in the field: ${field}")

case class InvalidConfigError(message: String)
  extends Exception(message, null)
