package com.beepboop.app.dataprovider

/* third party modules */
import com.beepboop.app.dataprovider.DataImporter.resolveBound
import com.beepboop.app.logger.Profiler
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}

import scala.util.Random
import javax.lang.model.util.Elements.Origin
import scala.io.Source

/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.MinizincModelListener
import com.beepboop.app.ParserUtil
import com.beepboop.app.{GrammarConverter, ParsedGrammar}
import com.beepboop.app.components.Variable
import com.beepboop.app.logger.LogTrait
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser, NewMinizincParserBaseListener}



sealed trait DataItemRegistry {
  def getValue(name: String): Any
}


object DataProvider extends LogTrait {
  private val rand = new Random()
  var solutionCount: Int = 0

  private var solutionNumber: Int = 0

  var solutionContexts: Vector[Map[String, Any]] = Vector()

  var variables: List[DataItem] = Nil
  var parameters: List[DataItem] = Nil

  private var variableCreatables: Option[List[Creatable]] = None

  def getSolutionNumber: Int = solutionNumber

  def setSolutionNumber(n: Int): Unit = {
    if (n >= 0 && n < solutionContexts.size) solutionNumber = n
  }


  def createRandomContext(): Map[String, Any] = {
    val allItems = variables ++ parameters

    allItems.map { item =>
      val randomValue = generateRandomValue(item)
      item.name -> randomValue
    }.toMap
  }

  private def generateRandomValue(item: DataItem): Any = {
    val exprType = getExpressionType(item)

    exprType match {
      case IntType =>
        getRandomInt(item)

      case BoolType =>
        rand.nextBoolean()

      case ListIntType =>
        val listSize = getCollectionSize(item)
        List.fill(listSize)(rand.nextInt(100) - 50)

      case SetIntType =>
        val setSize = rand.nextInt(5) + 1
        List.fill(setSize)(rand.nextInt(100)).toSet

      case _ =>
        0
    }
  }

  private def getRandomInt(item: DataItem): Int = {
    if (item.expr != null && item.expr.contains("..")) {
      try {
        val bounds = item.expr.split("\\.\\.")
        val allItems = variables ++ parameters
        val min = resolveBound(bounds(0), allItems)
        val max = resolveBound(bounds(1), allItems)

        if (max > min) rand.nextInt(max - min + 1) + min
        else min
      } catch {
        case _: Exception =>
          //Profiler.recordValue(s"RandomInt Resolution Fail: ${item.name}", 1)
          rand.nextInt(100)
      }
    } else {
      rand.nextInt(100)
    }
  }
  private def getCollectionSize(item: DataItem): Int = {
    solutionContexts.headOption.flatMap(_.get(item.name)) match {
      case Some(l: List[_]) => l.size
      case _ => 5
    }
  }

  def createSolutionContext(solutionIndex: Int): Map[String, Any] = {
    if (solutionIndex < 0 || solutionIndex >= solutionContexts.size) {
      if (solutionContexts.nonEmpty) solutionContexts.head
      else Map.empty
    } else {
      solutionContexts(solutionIndex)
    }
  }

  def getSolutionContext(solutionIndex: Int): Map[String, Any] = createSolutionContext(solutionIndex)

  def getValue(name: String): Any = {
    if (solutionContexts.isEmpty) {
      parameters.find(_.name == name).flatMap(p => Option(p.value))
        .getOrElse(throw new IllegalStateException(s"DataProvider not initialized or key '$name' not found."))
    } else {
      createSolutionContext(solutionNumber).get(name) match {
        case Some(v) => v
        case None => throw new NoSuchElementException(s"No value found for '$name' in solution $solutionNumber.")
      }
    }
  }

  def initalize(originalModelPath: String, dataPaths: List[String], solutionsPaths: List[String]): Unit = {
    if (dataPaths.size != solutionsPaths.size) {
      val msg = s"Count mismatch: ${dataPaths.size} data files vs ${solutionsPaths.size} solution files. " +
        "For distinct instances (Scenario B), they must be paired 1-to-1."
      error(msg)
      throw new IllegalArgumentException(msg)
    }

    val (modelParamsSchema, modelVarsSchema) = ModelParser.getDataItems(originalModelPath)
    this.variables = modelVarsSchema
    this.parameters = modelParamsSchema

    val allContextsBuffer = scala.collection.mutable.ListBuffer[Map[String, Any]]()

    dataPaths.zip(solutionsPaths).zipWithIndex.foreach { case ((dPath, sPath), idx) =>
      info(s"--- Loading Instance #${idx + 1} ---")

      val instanceParams = modelParamsSchema.map(_.copy())

      this.parameters = instanceParams
      DataImporter.importDataFile(dPath, instanceParams)

      val paramMap: Map[String, Any] = instanceParams
        .filter(p => p.value != null && p.value != None)
        .map(p => p.name -> p.value)
        .toMap

      val instanceSolutions: List[Map[String, Any]] = SolutionParser.parse(sPath, separator = ';', this.variables)

      info(s"    -> Loaded ${instanceSolutions.size} solutions.")

      instanceSolutions.foreach { solMap =>
        allContextsBuffer += (solMap ++ paramMap)
      }
    }

    this.solutionContexts = allContextsBuffer.toVector
    this.solutionCount = solutionContexts.size

    if (solutionCount > 0) {
      this.parameters = modelParamsSchema
    }

    if (solutionCount == 0) throw new IllegalStateException("No solutions loaded from any provided files.")

    info(s"Total: Loaded $solutionCount contexts across ${dataPaths.size} instances.")

    info("==================== DATA LOADING SUMMARY ====================")
    val previewCtx = if (solutionContexts.nonEmpty) solutionContexts.head else Map.empty

    (variables ++ parameters).sortBy(_.name).foreach { item =>
      val internalKind = if (item.isVar) "VAR" else "PAR"
      val internalType = getExpressionType(item)

      val valuePreview = previewCtx.get(item.name) match {
        case Some(l: List[_]) =>
          if (l.nonEmpty && l.head.isInstanceOf[Set[_]]) s"List[Set] (size=${l.size}, first=${l.head})"
          else s"List (size=${l.size})"
        case Some(s: Set[_]) => s"Set (size=${s.size})"
        case Some(v) => v.toString
        case None => "MISSING / NONE"
      }

      info(f"[$internalKind] ${item.name}%-20s | Type: ${internalType.toString}%-15s | Val: $valuePreview")
    }
    info("==============================================================\n")

    initializeCreatables(solutionContexts.head, modelParamsSchema)
  }

  private def initializeCreatables(sampleContext: Map[String, Any], paramSchema: List[DataItem]): Unit = {
    val groupedItems = sampleContext.groupBy { case (name, value) =>
      val schemaItem = variables.find(_.name == name).orElse(paramSchema.find(_.name == name))
      schemaItem match {
        case Some(item) => getExpressionType(item)
        case None => inferTypeFromValue(value)
      }
    }

    this.variableCreatables = Some(
      groupedItems.map { case (exprType, itemsMap) =>
        new RandomVariableFactory(exprType, itemsMap.keys.toList)
      }.toList
    )
  }

  def getExpressionType(item: DataItem): ExpressionType = {
    val typeStr = Option(item.detailedDataType)
      .map(_.dataType.toLowerCase)
      .getOrElse(item.dataType.toLowerCase)

    val isArray = Option(item.detailedDataType).exists(_.isArray)
    val isSet = Option(item.detailedDataType).exists(_.isSet)

    if (isArray && isSet) ListSetIntType
    else if (isArray) ListIntType
    else if (isSet) ListIntType
    else if (typeStr.contains("bool")) BoolType
    else if (typeStr.contains("int") || typeStr.contains("..")) IntType
    else {
      solutionContexts.headOption.flatMap(_.get(item.name)) match {
        case Some(_: Int) | Some(_: java.lang.Integer) => IntType
        case Some(_: Boolean) | Some(_: java.lang.Boolean) => BoolType
        case _ => UnknownType
      }
    }
  }

  private def inferTypeFromValue(value: Any): ExpressionType = value match {
    case _: Int | _: java.lang.Integer => IntType
    case _: Boolean | _: java.lang.Boolean => BoolType
    case l: List[_] if l.nonEmpty && l.head.isInstanceOf[Int] => ListIntType
    case l: List[_] if l.nonEmpty && l.head.isInstanceOf[Set[_]] => ListSetIntType
    case _: Set[_] => SetIntType
    case _ => UnknownType
  }

  def getVariableCreatables: List[Creatable] = {
    variableCreatables.getOrElse(throw new IllegalStateException("DataProvider not initialized."))
  }

  object ParameterRegistry {
    def getAll(): List[DataItem] = parameters
  }

  object VarRegistry {
    def dataVars: Vector[Map[String, Any]] = solutionContexts
    def getValue(name: String): Option[Any] = solutionContexts.lift(solutionNumber).flatMap(_.get(name))
  }
}

object ModelParser extends LogTrait {
  def getDataItems(originalModelPath: String): (List[DataItem], List[DataItem]) = {
    info(s"Reading model file $originalModelPath")
    val modelCode: String = Source.fromFile(originalModelPath).mkString
    val (_, mzparseTree) = ParserUtil.parseCode(
      code = modelCode,
      lexerFactory = (cs: CharStream) => new NewMinizincLexer(cs),
      parserFactory = (ts: TokenStream) => new NewMinizincParser(ts),
      startRuleInvoker = (p: NewMinizincParser) => p.model()
    )

    val input: CharStream = CharStreams.fromString(modelCode)
    val lexer = new NewMinizincLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val modelListener = new MinizincModelListener(tokens)

    new ParseTreeWalker().walk(modelListener, mzparseTree)
    val allDeclaredItems = modelListener.getDataItems

    (allDeclaredItems.filter(!_.isVar), allDeclaredItems.filter(_.isVar))
  }

  def deriveDataType(value: Any): (String, DataType) = {
    val typeName = value.getClass.getSimpleName.toLowerCase
    (typeName, DataType(typeName, false, false))
  }
}

object VarNameGenerator {
  private var counter: Int = 0
  def generateUniqueName(prefix: String = "tmp_var"): String = synchronized {
    val newName = s"${prefix}_${counter}"
    counter += 1
    newName
  }
}