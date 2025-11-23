import com.beepboop.app.{MinizincModelListener, ParserUtil}
import com.beepboop.parser.{MinizincGrammarLexer, MinizincGrammarParser, NewMinizincLexer, NewMinizincParser}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inspectors.forAll

import scala.io.Source
/*
class DataItemExtractionSuiteTest extends AnyFunSuite {

  val modelsToTest: Map[String, List[DataItem]] = Map(
    "test_simple_params.mzn" -> List(
      DataItem(name = "flights", dataType = "int", isVar = false),
      DataItem(name = "FLIGHT", dataType = "set of int", isVar = false),
      DataItem(name = "enabled", dataType = "bool", isVar = false)
    ),
    "test_array_params.mzn" -> List(
      DataItem(name = "airlines", dataType = "int", isVar = false),
      DataItem(name = "AIRLINE", dataType = "set of int", isVar = false),
      DataItem(name = "FLIGHT", dataType = "set of int", isVar = false),
      DataItem(name = "FA", dataType = "array[AIRLINE] of set of FLIGHT", isVar = false)
    ),
    "test_comprehension_params.mzn" -> List(
      DataItem(name = "FLIGHT", dataType = "set of int", isVar = false),
      DataItem(name = "TIME", dataType = "set of int", isVar = false),
      DataItem(name = "xCoor", dataType = "array[FLIGHT] of int", isVar = false),
      DataItem(name = "opDur", dataType = "array[FLIGHT] of int", isVar = false),
      DataItem(name = "ISet", dataType = "array[FLIGHT] of set of TIME", isVar = false)
    ),
    "test_no_params.mzn" -> List(
      DataItem(name = "x", dataType = "var int", isVar = true),
      DataItem(name = "y", dataType = "var set of 1..10", isVar = true)
    )
  )

  private def extractDataItemsFromFile(filename: String): List[DataItem] = {
    val modelCode = Source.fromFile(filename).mkString
    val input: CharStream = CharStreams.fromString(modelCode)
    val lexer = new NewMinizincLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new NewMinizincParser(tokens)
    val tree = parser.model()

    val listener = new MinizincModelListener(tokens, false)
    new ParseTreeWalker().walk(listener, tree)

    listener.getDataItems
  }

  forAll(modelsToTest.keys.toList) { modelName =>
    test(s"Model '$modelName' should be parsed correctly, extracting the right data items") {
      val modelPath = s"src/test/resources/models/$modelName"
      val expectedDataItems = modelsToTest(modelName)

      val result = extractDataItemsFromFile(modelPath)

      assert(result.size == expectedDataItems.size)
      assert(result.toSet == expectedDataItems.toSet)
    }
  }
}

 */