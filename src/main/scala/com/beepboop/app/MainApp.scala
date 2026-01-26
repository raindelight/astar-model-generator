package com.beepboop.app

import com.beepboop.app.astar.AStar
import com.beepboop.app.components.*
import com.beepboop.app.cpicker.{ConstraintPicker, ConstraintSaver, Runner}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.Profiler
import com.beepboop.app.mutations.{AllMutations, ExpressionGenerator, MutationEngine}
import com.typesafe.scalalogging.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}
import com.beepboop.app.dataprovider.PersistenceManager
import com.beepboop.app.experiment.{ModelLoader, MultiTargetMetricReporter, SimilarityCSVExporter}
import com.beepboop.app.utils.AppConfig
import debugger.VisualDebugger
//import com.beepboop.app.utils.{GeneratorConfig, ArgumentParser}
import com.beepboop.app.utils.ArgumentParser
//import mainargs.{main, arg, ParserForClass, Flag}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success}

// logging
import com.beepboop.app.logger.LogTrait


object MainApp extends LogTrait {

  def main(args: Array[String]): Unit = {

    val configOpt = ArgumentParser.parse(args)
    if (configOpt.isEmpty) return
    val config = configOpt.get

    com.beepboop.app.dataprovider.ConfigLoader.initialize(config.configPath)

    info("--- Step 1: Configuration ---")
    info(s"Model: ${config.modelPath}")
    info(s"Max Iterations: ${config.maxIterations}")
    info(s"Output CSV: ${config.outputCsv}")
    info(s"Checkpoint File: ${config.checkpointFile}")
    info(s"Resume mode: ${config.resume}")
    info("Loading parsed constraint grammar...")


    if (config.analyze) {
      info("--- Launching similarity analyzer ---")
      val targetModel = ModelLoader.loadModel(config.analyzeModel)
      val reporter = new MultiTargetMetricReporter(targetModel)
      val snapshot = PersistenceManager.loadAStarState(config.checkpointFile).get
      val fullReport = reporter.generateFullReport(snapshot)

      val targetLabels = targetModel.targetConstraints.zipWithIndex.map {
        case (t, i) => s"${t.getClass.getSimpleName}_$i"
      }

      SimilarityCSVExporter.dumpToCSV(fullReport, targetLabels, "astar_metrics.csv")
      info(s"Analysis complete. Structural matrix saved to astar_metrics.csv")

      val overallBest = fullReport.flatMap(r => r.scores.map(s => (r.nodeString, s._1, s._2))).maxBy(_._3)
      info(s"Highest logical match found: ${overallBest._1} aligns with ${overallBest._2} (${(overallBest._3 * 100).toInt}%)")
      return
    }


    val internalGrammar: ParsedGrammar = GrammarConverter.parseConstraintGrammar()
    debug(ParsedGrammar.toString)

    DataProvider.initalize(
      config.modelPath,
      config.dataPath,
      config.solutionsPath
    )
    if (config.debug) {
      info("--- Launching Visual Debugger ---")
      VisualDebugger.launch(config.checkpointFile)
      return;
    }



    info("\n--- Step 2: Invoking Mutation Engine ---")
    val mutationEngine = new MutationEngine(AllMutations.mutations)

    info(s"Initializing A* Search with heuristic mode: ${config.heuristic}")
    val searcher = new AStar(internalGrammar, config.heuristic)

    if (config.resume) {
      info(s"Attempting to resume from ${config.checkpointFile}...")
      PersistenceManager.loadAStarState(config.checkpointFile) match {
        case Success(snapshot) =>
          searcher.restoreState(snapshot)
          info("Resume successful.")
        case Failure(e) =>
          warn(s"Failed to load checkpoint: ${e.getMessage}. Starting from scratch.")
      }
    }

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      val snapshot = searcher.getSnapshot
      if (snapshot.openSetItems.nonEmpty || snapshot.visitedItems.nonEmpty) {
        PersistenceManager.performEmergencyBackup(snapshot, config.checkpointFile, config.outputCsv)
      }
    }))

    val initialExpr = ExpressionGenerator.generate(requiredType = BoolType, maxDepth = 2)
      .getOrElse(Constant(value = false))

    info("--- Step 3: Starting Optimization Loop ---")

    val result = searcher.findOptimalModel(
      initialConstraint = initialExpr,
      availableVars = DataProvider.variables,
      dataPars = DataProvider.parameters,
      maxIterations = config.maxIterations,
      saveInterval = config.saveInterval,
      checkpointFile = config.checkpointFile,
      outputCsvFile = config.outputCsv
    )

    result.foreach { nodes =>
      info(s"Search completed. Saving ${nodes.size} constraints to CSV...")
      PersistenceManager.saveConstraintsToCSV(nodes, config.outputCsv)
    }


    info("--- Step 4. Starting constraint picking ---")

    ConstraintSaver.setConfig(config)

    ConstraintPicker.setConfig(config)
    /* vvv comment if no gurobi license present vvv */
    //ConstraintPicker.runInitial(result.get)




    Profiler.report()
    Profiler.reset()





  }
}