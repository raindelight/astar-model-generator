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

