package com.beepboop.app

import com.beepboop.app.astar.AStar
import com.beepboop.app.components.{BinaryExpression, BoolType, ComponentRegistry, Constant, EqualOperator, Expression, Variable}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.mutations.{AllMutations, ExpressionGenerator, MutationEngine}
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser, NewMinizincParserBaseListener}
import com.typesafe.scalalogging.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}

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
      "models/models/mznc2024_probs/accap/accap.mzn",
      "models/models/mznc2024_probs/accap/accap_a3_f20_t10.json",
      "models/accap_sols_a10.csv"
    )


    info("--- TEST: Sprawdzanie wartości po DataImporter ---")
    
    info("--- Parametry (z pliku .json): ---")
    DataProvider.parameters.foreach { item =>
      if (item.value != None) {
        info(s"  -> ${item.name}: ${item.value} (Typ: ${item.value.getClass.getSimpleName})")
      } else {
        warn(s"  -> ${item.name}: BRAK WARTOŚCI")
      }
    }
    
    info("--- Zmienne (z pliku .csv): ---")
    DataProvider.variables.foreach { item =>
      if (item.value != None) {
        info(s"  -> ${item.name}: ${item.value} (Typ: ${item.value.getClass.getSimpleName})")
      } else {
        warn(s"  -> ${item.name}: BRAK WARTOŚCI")
      }
    }
    info("--- KONIEC TESTU ---")



    info("\n--- Step 2: Invoking Mutation Engine ---")
    val mutationEngine = new MutationEngine(AllMutations.mutations)


    val searcher = new AStar(internalGrammar)
    val t = searcher.findOptimalModel(ExpressionGenerator.generate(requiredType = BoolType, maxDepth = 2).getOrElse(Constant(value = false)), DataProvider.variables, DataProvider.parameters)
    t.foreach(c => {
      info(c.toString)
    })
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