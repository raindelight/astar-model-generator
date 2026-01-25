package com.beepboop.app.logger

// third party
import com.beepboop.app.dataprovider.ClassLogConfig
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
// own
import com.beepboop.app.logger.*
import com.beepboop.app.dataprovider.ConfigLoader
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait LogTrait {

  @transient
  protected lazy val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  private lazy val globalLogEnable: Boolean = ConfigLoader.settings.logging.enabled
  private lazy val logConfigDebug: Boolean = ConfigLoader.settings.logging.logDebug

  private lazy val logConfig: ClassLogConfig = {
    val tLogConfig = ConfigLoader.getClassLogConfig(getClass.getName)
    if (logConfigDebug) {
      logger.debug(s"Config for: ${getClass.getName} | level: ${tLogConfig.level} | gEnable: $globalLogEnable | enabled: ${tLogConfig.enabled}")
    }
    tLogConfig
  }

  private def shouldLog(level: String): Boolean = {
    if (!globalLogEnable || !logConfig.enabled) return false

    val order = Seq("DEBUG", "INFO", "WARN", "ERROR")
    val configIdx = order.indexOf(logConfig.level.toUpperCase)
    val msgIdx = order.indexOf(level.toUpperCase)

    if (configIdx == -1 || msgIdx == -1) return false

    msgIdx >= configIdx
  }

  def debug(message: String): Unit = if (shouldLog("DEBUG")) logger.debug(message)
  def info(message: String): Unit = if (shouldLog("INFO")) logger.info(message)
  def warn(message: String): Unit = if (shouldLog("WARN")) logger.warn(message)
  def error(message: String): Unit = if (shouldLog("ERROR")) logger.error(message)
}
