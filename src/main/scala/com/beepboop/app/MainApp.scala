package com.beepboop.app

import com.beepboop.app.astar.AStar
import com.beepboop.app.components.{BinaryExpression, BoolType, ComponentRegistry, Constant, EqualOperator, Expression, Variable}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.Profiler
import com.beepboop.app.mutations.{AllMutations, ExpressionGenerator, MutationEngine}
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser, NewMinizincParserBaseListener}
import com.typesafe.scalalogging.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}
import com.beepboop.app.dataprovider.PersistenceManager
import com.beepboop.app.utils.ArgumentParser

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success}

// logging
import com.beepboop.app.logger.LogTrait


object MainApp extends LogTrait {

  def main(args: Array[String]): Unit = {

    val config = ArgumentParser.parse(args) match {
      case Some(cfg) => cfg
      case None => return
    }

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

    Profiler.report()
    Profiler.reset()
    /*
    var mutatedExpression = initialExpression


    info(s"0 Original Expression: ${initialExpression}")
    for(no <- 0 to 10) {
      mutatedExpression = mutationEngine.mutate(mutatedExpression)
      info(s"$no Mutated Expression: ${mutatedExpression}")
      var satisfied = 0

      for(i <- 0 to 5000 - 1){
        val context = DataProvider.createSolutionContext(i)
        if(mutatedExpression.eval(context) == true) satisfied += 1
      }
      info(s"$no Satisfied $satisfied / 5000 (${(satisfied/5000)*100}%)")
    }
  */



    
  }
}