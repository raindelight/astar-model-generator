package com.beepboop.app.utils

import mainargs.{arg, ParserForClass, Flag}

case class GeneratorConfig(
                            @arg(positional = true, doc = "Path to the .mzn model file")
                            modelPath: String,

                            @arg(name = "data", short = 'D', doc = "Paths to .json/.dzn data files (repeat for multiple)")
                            dataPaths: Seq[String],

                            @arg(name = "solutions", short = 's', doc = "Path to the known solutions CSV")
                            solutionsPath: Seq[String],

                            @arg(short = 'i', doc = "Maximum number of A* iterations")
                            maxIterations: Int = 50,

                            @arg(doc = "How often to save checkpoint/CSV")
                            saveInterval: Int = 100,

                            @arg(name = "heuristic", short = 'e', doc = "Heuristic method: avg (default), min, max, mse, var")
                            heuristic: String = "avg",

                            @arg(short = 'o', doc = "Output CSV file path")
                            outputCsv: String = "generated_constraints.csv",

                            @arg(short = 'c', doc = "Checkpoint file path")
                            checkpointFile: String = "astar_checkpoint.bin",

                            @arg(short = 'r', doc = "Resume from checkpoint if exists")
                            resume: Flag,

                            @arg(name = "debug", short = 'd', doc = "Launch Visual Debugger GUI instead of running search")
                            debug: Flag,

                            @arg(name = "gurobi-license", doc = "Path to gurobi license file")
                            gurobiLicense: String = ""
                          )

case class AppConfig(
                      modelPath: String,
                      dataPath: List[String],
                      solutionsPath: List[String],
                      maxIterations: Int,
                      saveInterval: Int,
                      heuristic: String,
                      outputCsv: String,
                      checkpointFile: String,
                      resume: Boolean,
                      debug: Boolean,
                      gurobiLicense: String
                    )

object ArgumentParser {

  private val parser = ParserForClass[GeneratorConfig]

  private val HelpHeader =
    """
      |AStar Model Generator v1.0
      |Generates MiniZinc constraints using A* search algorithm based on provided data.
      |
      |Usage: sbt "run <modelPath> <dataPath> [options]"
      |""".stripMargin

  private val HelpFooter =
    """
      |EXAMPLES:
      |  Basic run:
      |    sbt "run models/accap.mzn models/accap.json -s models/sols.csv"
      |
      |  Resume:
      |    sbt "run models/accap.mzn models/accap.json -s models/sols.csv -r"
      |""".stripMargin

  def parse(args: Array[String]): Option[AppConfig] = {

    if (args.contains("--help") || args.contains("-h")) {

      val rawLibraryHelp = parser.helpText(customName = "astar-generator")

      val optionsList = rawLibraryHelp.linesIterator.drop(1).mkString("\n")

      println(HelpHeader)
      println("Options:")
      println(optionsList)
      println(HelpFooter)

      return None
    }

    parser.constructEither(args.toSeq) match {
      case Right(cli) =>
        Some(AppConfig(
          modelPath = cli.modelPath,
          dataPath = cli.dataPaths.toList,
          solutionsPath = cli.solutionsPath.toList,
          maxIterations = cli.maxIterations,
          saveInterval = cli.saveInterval,
          heuristic = cli.heuristic,
          outputCsv = cli.outputCsv,
          checkpointFile = cli.checkpointFile,
          resume = cli.resume.value,
          debug = cli.debug.value,
          gurobiLicense = cli.gurobiLicense
        ))

      case Left(errorMsg) =>
        println(s"Error parsing arguments: $errorMsg")
        println("\nRun with --help to see usage.")
        None
    }
  }
}