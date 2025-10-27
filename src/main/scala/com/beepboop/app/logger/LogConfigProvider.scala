package com.beepboop.app.logger


import org.yaml.snakeyaml.Yaml
import scala.jdk.CollectionConverters._
import java.io.FileInputStream


case class ClassLogConfig(level: String, enable: Boolean)



private def toScala(value: Any): Any = value match {
  case m: java.util.Map[_, _] =>
    m.asScala.view.mapValues(toScala).toMap
  case l: java.util.List[_] =>
    l.asScala.map(toScala).toList
  case other => other
}


// From map to ClassLogConfig
object ClassLogConfigMapper {
  def fromMap(data: Map[String, Object]): ClassLogConfig = {
    val level: String = data.getOrElse("level", "INFO").toString
    val enable: Boolean = data.getOrElse("enabled", false).toString.toBoolean
    ClassLogConfig(level, enable)
  }
}

object LogConfigProvider {
  private val configFile = "src/main/resources/logging.yml"

  // once
  private lazy val config: Map[String, Object] = {
    val yaml = new Yaml()
    val input = new FileInputStream(configFile)
    val data = yaml.load[java.util.Map[String, Object]](input)
    input.close()

    toScala(data).asInstanceOf[Map[String, Object]]
  }

  // getters
  def logLevel: String = config.getOrElse("level", "INFO").toString
  def logEnabled: Boolean = config.getOrElse("enabled", true).toString.toBoolean
  def logConfigDebugEnabled: Boolean = config.getOrElse("log_debug", false).toString.toBoolean
  def classLogConfig(className: String): ClassLogConfig = {
    // class name returned using getClass.getName have an extra $ at the end
    val raw = config.getOrElse(className.stripSuffix("$"), Map[String, Object]()).asInstanceOf[Map[String, Object]]
    ClassLogConfigMapper.fromMap( raw )
  }

}

