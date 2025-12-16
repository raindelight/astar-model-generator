package com.beepboop.app.cpicker

import com.beepboop.app.utils.AppConfig

import java.io.File
import java.nio.file.Path
import scala.sys.process.*
import spray.json.*
import DefaultJsonProtocol.*

class Runner(config: AppConfig) {
  def run(path: Path): Long = {


    val test_command = Seq(
      "pwd"
    )
    println(test_command.!!)

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
    println(command.mkString(" "))

    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = ProcessLogger(stdout append _, stderr append _)
    val status = Process(command, new File("."), "GRB_LICENSE_FILE" -> config.gurobiLicense) ! logger

    if (status == 0) {
      val parsedOutput = stdout.toString().replace("}{", "}\n{").split("\n")
      println(parsedOutput.head)
      val jsonObjects = parsedOutput.map(o => o.parseJson.asJsObject)


      val count = jsonObjects.count(j => j.fields.get("output") match {
        case Some(jsValue) => true
        case None => false
      })
      println(count)
      count
    } else {
      println(stdout)
      println(stderr)
      0
    }
  }
}