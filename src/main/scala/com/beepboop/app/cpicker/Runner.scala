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
    var totalSolutions: Long = 0
    var failureOccurred = false

    for (dataPath <- config.dataPath if !failureOccurred) {

      val command = Seq(
        "minizinc",
        "--solver", "gurobi",
        "-I", currentWorkingDir,
        "--json-stream",
        "--param-file-no-push", "config.mpc",
        "--parallel", "1",
        "--all-solutions",
        path.toAbsolutePath.toString,
        dataPath
      )

      debug(s"Running against instance $dataPath: ${command.mkString(" ")}")

      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val logger = ProcessLogger(stdout append _, stderr append _)

      val status = Process(command, new File("."), "GRB_LICENSE_FILE" -> config.gurobiLicense) ! logger

      if (status == 0) {
        val parsedOutput = stdout.toString().replace("}{", "}\n{").split("\n")
        val jsonObjects = parsedOutput.filter(_.trim.nonEmpty).map(o => o.parseJson.asJsObject)

        val count = jsonObjects.count(j => j.fields.get("output") match {
          case Some(jsValue) => true
          case None =>  {
            if (!j.fields.contains("type")) Profiler.recordValue(s"Invalid mzn output ($dataPath)", 1)
            false
          }
        })

        debug(s"  -> Solutions found: $count")
        totalSolutions += count
      } else {
        error(s"MiniZinc failed for instance: $dataPath")
        debug(stdout.toString)
        debug(stderr.toString)
        failureOccurred = true
      }
    }

    if (failureOccurred) None else Some(totalSolutions)
  }
}