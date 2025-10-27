package com.beepboop.app.logger

// third party
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
// own
import com.beepboop.app.logger.*


trait LogTrait {

  @transient
  protected lazy val logger: Logger = {
    Logger(LoggerFactory.getLogger(getClass.getName))
  }

  private lazy val globalLogEnable: Boolean = LogConfigProvider.logEnabled
  private lazy val logConfigDebug: Boolean = LogConfigProvider.logConfigDebugEnabled
  private lazy val logConfig: ClassLogConfig = {
    val tLogConfig = LogConfigProvider.classLogConfig(getClass.getName)
    if(logConfigDebug) logger.debug(s"Config for: ${getClass.getName} |  level: ${tLogConfig.level} | gEnable: ${globalLogEnable} | enabled: ${tLogConfig.enable}")
    tLogConfig
  }


  private def shouldLog(level: String): Boolean = {
    if (!globalLogEnable) return false
    if (!logConfig.enable) return false

    val order = Seq("DEBUG", "INFO", "WARN", "ERROR")
    val configIdx = order.indexOf(logConfig.level.toUpperCase)
    val msgIdx = order.indexOf(level.toUpperCase)

    msgIdx >= configIdx
  }


  def debug(message: String): Unit = {
    if (shouldLog("DEBUG")) logger.debug(message)
  }
  def info(message: String): Unit = {
    if (shouldLog("INFO")) logger.info(message)
  }
  def warn(message: String): Unit = {
    if (shouldLog("WARN")) logger.warn(message)
  }
  def error(message: String): Unit = {
    if (shouldLog("ERROR")) logger.error(message)
  }
}

