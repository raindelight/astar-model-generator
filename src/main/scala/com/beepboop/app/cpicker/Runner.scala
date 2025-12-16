package com.beepboop.app.cpicker

import com.beepboop.app.utils.AppConfig

import java.io.File
import java.nio.file.Path
import scala.sys.process.*
import spray.json.*
import DefaultJsonProtocol.*
import com.beepboop.app.logger.{LogTrait, Profiler}

class Runner(config: AppConfig) extends LogTrait {
  def run(path: Path): Option[Long] = {
    val currentWorkingDir = System.getProperty("user.dir")

    val command = Seq(
      "minizinc",
      "--solver", "gurobi",
      "-I", currentWorkingDir,
      "--json-stream",
      "--time-limit", "20000",
      "--param-file-no-push", "config.mpc",
      "--parallel", "1",
      "--all-solutions",
      path.toAbsolutePath.toString,
      config.dataPath
    )
    debug(s"Running: ${command.mkString(" ")}")

    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = ProcessLogger(stdout append _, stderr append _)
    val status = Process(command, new File("."), "GRB_LICENSE_FILE" -> config.gurobiLicense) ! logger

    if (status == 0) {
      val parsedOutput = stdout.toString().replace("}{", "}\n{").split("\n")
      debug(s"Output head: ${parsedOutput.head}")
      val jsonObjects = parsedOutput.map(o => o.parseJson.asJsObject)

      val count = jsonObjects.count(j => j.fields.get("output") match {
        case Some(jsValue) => true
        case None =>  {
          Profiler.recordValue("Invalid mzn", 1)
          false
        }
      })
      Some(count)
    } else {
      debug(stdout.toString)
      debug(stderr.toString)
      None
    }
  }
}