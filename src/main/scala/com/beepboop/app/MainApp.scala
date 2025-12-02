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

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// logging
import com.beepboop.app.logger.LogTrait



object MainApp extends LogTrait {

  def main(args: Array[String]): Unit = {
    info("--- Step 1: Configuration ---")
    info("Loading hardcoded constraint grammar...")
    val internalGrammar: ParsedGrammar = GrammarConverter.parseConstraintGrammar()
    debug(ParsedGrammar.toString)


    DataProvider.initalize(
      "models/mznc2024_probs/accap/accap.mzn",
      "models/mznc2024_probs/accap/accap_a3_f20_t10.json",
      "models/accap_sols_a10.csv"
    )

    info("\n--- Step 2: Invoking Mutation Engine ---")
    val mutationEngine = new MutationEngine(AllMutations.mutations)


    val searcher = new AStar(internalGrammar)

    if (new java.io.File("astar_checkpoint.bin").exists() && false) {
      searcher.loadState("astar_checkpoint.bin")
    }

    val t = searcher.findOptimalModel(ExpressionGenerator.generate(requiredType = BoolType, maxDepth = 2).getOrElse(Constant(value = false)), DataProvider.variables, DataProvider.parameters)
    t.foreach(nodes => {
      debug(s"Found ${nodes.size} nodes. Saving results to CSV...")
      val f = new java.io.File("generated_constraints.csv")
      if (f.exists()) debug("SUCCESS! File exists on disk.")
      info("Saving results to CSV...")
      PersistenceManager.saveConstraintsToCSV(nodes, "generated_constraints.csv")
    })

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