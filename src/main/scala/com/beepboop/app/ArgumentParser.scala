package com.beepboop.app.utils
import java.nio.file.Path
import mainargs.{main, arg, Flag}


@main
case class AppConfig(
                      @arg(positional=true, doc="Path to a model")
                      modelPath: String,
                      dataPath: String = "models/mznc2024_probs/accap/accap_a3_f20_t10.json",
                      solutionsPath: Option[String] = Some("models/accap_sols_a10.csv"),
                      maxIterations: Int = 500,
                      saveInterval: Int = 100,
                      outputCsv: String = "generated_constraints.csv",
                      checkpointFile: String = "astar_checkpoint.bin",
                      resume: Boolean = false
                    )
/*
object ArgumentParser {

  def parse(args: Array[String]): Option[AppConfig] = {
    parseRec(args.toList, AppConfig())
  }

  @scala.annotation.tailrec
  private def parseRec(args: List[String], config: AppConfig): Option[AppConfig] = {
    args match {
      case Nil => Some(config)

      case "--model" :: value :: tail =>
        parseRec(tail, config.copy(modelPath = value))

      case "--data" :: value :: tail =>
        parseRec(tail, config.copy(dataPath = value))

      case "--solutions" :: value :: tail =>
        parseRec(tail, config.copy(solutionsPath = Some(value)))

      case "--max-iter" :: value :: tail =>
        parseRec(tail, config.copy(maxIterations = value.toInt))

      case "--save-interval" :: value :: tail =>
        parseRec(tail, config.copy(saveInterval = value.toInt))

      case "--out" :: value :: tail =>
        parseRec(tail, config.copy(outputCsv = value))

      case "--checkpoint" :: value :: tail =>
        parseRec(tail, config.copy(checkpointFile = value))

      case "--resume" :: tail =>
        parseRec(tail, config.copy(resume = true))

      case "--help" :: _ =>
        printHelp()
        None

      case unknown :: _ =>
        println(s"Unknown argument: $unknown")
        printHelp()
        None
    }
  }

  def printHelp(): Unit = {
    println(
      """
        |Minizinc Optimal Constraint Generator
        |
        |Usage: java -jar app.jar [options]
        |     : sbt "run [options]"
        |
        |Options:
        |  --model <path>          Path to the .mzn model file
        |  --data <path>           Path to the .json/.dzn data file
        |  --solutions <path>      Path to the known solutions CSV (optional)
        |  --max-iter <int>        Maximum number of A* iterations (default: 500)
        |  --save-interval <int>   How often to save checkpoint/CSV (default: 100)
        |  --out <path>            Output CSV file path
        |  --checkpoint <path>     Checkpoint file path
        |  --resume                Resume from checkpoint if exists
        |  --help                  Show this message
        |""".stripMargin)
  }
}
*/