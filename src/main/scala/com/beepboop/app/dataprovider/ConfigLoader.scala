package com.beepboop.app.dataprovider

import com.beepboop.app.mutations.Mutation
import pureconfig.ConfigReader
import pureconfig.ConfigSource

case class ClassLogConfig(
                           level: String = "INFO",
                           enabled: Boolean = false
                         ) derives ConfigReader

case class LogConfig(
                      enabled: Boolean = true,
                      logDebug: Boolean = false,
                      classes: Map[String, ClassLogConfig]
                    ) derives ConfigReader

case class AlgorithmConfig(
                            expressionWeights: Map[String, Double],
                            mutations: List[Mutation],
                            logging: LogConfig
                          ) derives ConfigReader

object ConfigLoader {
  import pureconfig.module.yaml.*

  private var _settings: Option[AlgorithmConfig] = None

  def initialize(path: String): Unit = {
    val loaded = YamlConfigSource.file(path).loadOrThrow[AlgorithmConfig]
    _settings = Some(loaded)
    println(s"Configuration loaded from: $path")
  }

  def settings: AlgorithmConfig = {
    _settings match {
      case Some(s) => s
      case None =>
        println("WARNING: ConfigLoader not initialized explicitly. Loading default 'config.yaml'.")
        initialize("config.yaml")
        _settings.get
    }
  }

  def getWeight(componentName: String): Double = {
    settings.expressionWeights.getOrElse(componentName, 0.0)
  }

  def getClassLogConfig(className: String): ClassLogConfig = {
    settings.logging.classes.getOrElse(className.stripSuffix("$"), ClassLogConfig())
  }
}