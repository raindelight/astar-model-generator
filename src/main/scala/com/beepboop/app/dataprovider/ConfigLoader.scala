package com.beepboop.app.dataprovider

import com.beepboop.app.mutations.Mutation
import pureconfig.ConfigReader


case class ClassLogConfig(
                           level: String = "INFO",
                           enabled: Boolean = false
                         )derives ConfigReader


case class LogConfig(
                      enabled: Boolean = true,
                      logDebug: Boolean = false,
                      classes: Map[String, ClassLogConfig]
                    )derives ConfigReader


case class AlgorithmConfig(
                      expressionWeights: Map[String, Double],
                      mutations: List[Mutation],
                      logging: LogConfig
                    ) derives ConfigReader


object ConfigLoader {
  import pureconfig.module.yaml._

  val settings: AlgorithmConfig = YamlConfigSource.file("config.yaml")
    .loadOrThrow[AlgorithmConfig]

  def getWeight(componentName: String): Double = {
    settings.expressionWeights.getOrElse(componentName, 0.0)
  }

  def getClassLogConfig(className: String): ClassLogConfig = {
    settings.logging.classes.getOrElse(className.stripSuffix("$"), ClassLogConfig())
  }
}