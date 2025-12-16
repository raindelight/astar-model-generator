package com.beepboop.app

import com.beepboop.app.astar.AStar
import com.beepboop.app.components.{BinaryExpression, BoolType, ComponentRegistry, Constant, EqualOperator, Expression, GreaterOperator, Variable}
import com.beepboop.app.cpicker.{ConstraintPicker, ConstraintSaver, Runner}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.Profiler
import com.beepboop.app.mutations.{AllMutations, ExpressionGenerator, MutationEngine}
import com.typesafe.scalalogging.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}
import com.beepboop.app.dataprovider.PersistenceManager
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

    info("--- Step 1: Configuration ---")
    info(s"Model: ${config.modelPath}")
    info(s"Max Iterations: ${config.maxIterations}")
    info(s"Output CSV: ${config.outputCsv}")
    info(s"Checkpoint File: ${config.checkpointFile}")
    info(s"Resume mode: ${config.resume}")
    info("Loading parsed constraint grammar...")
    val internalGrammar: ParsedGrammar = GrammarConverter.parseConstraintGrammar()
    debug(ParsedGrammar.toString)

    DataProvider.initalize(
      config.modelPath,
      config.dataPath,
      config.solutionsPath.getOrElse("models/accap_sols_a10.csv")
    )
    if (config.debug) {
      info("--- Launching Visual Debugger ---")
      VisualDebugger.launch(config.checkpointFile)
      return;
    }

    info("\n--- Step 2: Invoking Mutation Engine ---")
    val mutationEngine = new MutationEngine(AllMutations.mutations)

    val searcher = new AStar(internalGrammar)

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
    ConstraintPicker.runInitial(result.get)




    Profiler.report()
    Profiler.reset()





  }
}