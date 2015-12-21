package edu.thu.ss.logic.util

import org.slf4j.LoggerFactory
import org.slf4j.impl.StaticLoggerBinder
import org.apache.log4j.PropertyConfigurator
import org.slf4j.Logger

trait Logging {
  // Make the log field transient so that objects with Logging can
  // be serialized and used on another machine
  @transient protected var _log: Logger = null

  // Method to get the logger name for this object
  protected def logName = {
    // Ignore trailing $'s in the class names for Scala objects
    this.getClass.getName.stripSuffix("$")
  }

  // Method to get or create the logger for this object
  protected def log: Logger = {
    if (_log == null) {
      _log = LoggerFactory.getLogger(logName)
    }
    _log
  }

}