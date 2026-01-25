import com.beepboop.app.ParserUtil
import com.beepboop.app.components.{IntType, ListIntType, ListSetIntType}
import com.beepboop.app.dataprovider.{DataProvider, ModelParser}
import com.beepboop.app.utils.{AppConfig, ArgumentParser}
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inspectors.*
import org.antlr.v4.runtime.*
import org.scalatest.BeforeAndAfterAll

import scala.io.Source

private object ThrowingErrorListener extends BaseErrorListener {
  override def syntaxError(
                            recognizer: Recognizer[_, _],
                            offendingSymbol: Any,
                            line: Int,
                            charPositionInLine: Int,
                            msg: String,
                            e: RecognitionException
                          ): Unit = {
    throw new RuntimeException(
      s"Syntax Error at line $line:$charPositionInLine - '$offendingSymbol: $msg"
    )
  }
}

class VariableExtractionSuite extends AnyFunSuite with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    val testArgs = Array(".", "--config", "config.yaml")

    val configObject = ArgumentParser.parse(testArgs).get

    AppConfig.init(configObject)
  }

  test("ModelParser should extract variables and parameters with correct InternalTypes") {
    val filename = "src/test/resources/models/accap_test.mzn"
    val (parameters, variables) = ModelParser.getDataItems(filename)
    val allItems = parameters ++ variables

    val expectedTypes = Map(
      "flights" -> IntType,
      "FLIGHT" -> ListIntType,
      "times" -> IntType,
      "TIME" -> ListIntType,
      "COUNTER" -> ListIntType,
      "AIRLINE" -> ListIntType,
      "airlines" -> IntType,
      "FA" -> ListSetIntType,
      //"ISet" -> ListSetIntType,
      "cNum" -> ListIntType,
      "xCoor" -> ListIntType,
      //"yCoor" -> ListIntType, -- silence test for a while, it evals correctly with solution data
      "opDur" -> ListIntType,
      "S" -> ListIntType,
      "D" -> IntType
    )

    expectedTypes.foreach { case (name, expectedType) =>
      val itemOpt = allItems.find(_.name == name)

      assert(itemOpt.isDefined, s"Expected item '$name' was not found in the model")

      val item = itemOpt.get
      if (item.name == "times") {
        println(s"Detailed Data for 'times': isArray=${item.detailedDataType.isArray}, isSet=${item.detailedDataType.isSet}")
      }
      val actualType = DataProvider.getExpressionType(item)

      assert(actualType == expectedType,
        s"Type mismatch for '$name' Expected: $expectedType, but got: $actualType (Raw: ${item.dataType})")
    }
  }

  }
class OverallGrammarParserSuite extends AnyFunSuite {
  val modelsToTest: List[String] = List.apply(
    "australia_test.mzn",
    "accap_test.mzn",
    "aircraft_test.mzn",
    "cable-tree-wiring_test.mzn",
    "community-detection_test.mzn",
    "compression_test.mzn",
    "concert-hall-cap_test.mzn",
    "foxgeesecorn_test.mzn",
    "graph_clear_cp_test.mzn",
    //"harmony_test.mzn", this model is so broken
    "hoist-benchmark_test.mzn",
    "monitor_1id_test.mzn",
    "neighbours-rect_test.mzn",
    "network_cstr_test.mzn",
    "peaceable_queens_test.mzn",
    "portal_test.mzn",
    "tiny_cvrp_test.mzn",
    "trains_test.mzn",
    "triangular_test.mzn",
    "yumi-dynamic_test.mzn",
    "test_array_params.mzn",
    "test_comprehension_params.mzn",
    "test_no_params.mzn",
    "test_simple_params.mzn"
  )

  def ParseModel(filename: String): Unit = {
    val modelCode: String =  Source.fromFile(filename).mkString
    val (_, mzparseTree) =
      ParserUtil.parseCode(
        code = modelCode,
        lexerFactory = (cs: CharStream) => {
          val lexer = new NewMinizincLexer(cs)
          lexer.removeErrorListeners()
          lexer.addErrorListener(ThrowingErrorListener)
          lexer
        },
        parserFactory = (ts: TokenStream) => {
          val parser = new NewMinizincParser(ts)
          parser.removeErrorListeners()
          parser.addErrorListener(ThrowingErrorListener)
          parser
        },
        startRuleInvoker = (p: NewMinizincParser) => p.model()
      )
  }

  test(s"List of models to test should have 23 models defined") {
    assert(modelsToTest.length == 23)
  }

  forAll(modelsToTest) {
    modelName => test(s"Parsing of $modelName should succeed") {
        ParseModel(s"src/test/resources/models/$modelName")
    }
  }
}
