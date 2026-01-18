package com.beepboop.app.dataprovider

/* third party modules */
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}

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
  var solutionCount: Int = 0
  private var solutionNumber: Int = 0
  private var originalModelPath: Option[String] = None
  private var dataPath: Option[String] = None
  private var solutionsPath: Option[String] = None

  // These will hold the raw data after initialization
  var parameters: List[DataItem] = Nil
  var variables: List[DataItem] = Nil
  var tmpVars: List[DataItem] = List()
  private var variableCreatables: Option[List[Creatable]] = None
  var solutionContexts: Vector[Map[String, Any]] = Vector();


  def getSolutionNumber: Int = solutionNumber

  def createSolutionContext(solutionNumber: Int): Map[String, Any] =  {
    val solutionMap: Map[String, Any] = VarRegistry.dataVars(solutionNumber)
    val parameterMap: Map[String, Any] = ParameterRegistry.dataPars.map(di => di.name -> di.value).toMap
    val tMap = solutionMap ++ parameterMap
    tMap
  }

  def getSolutionContext(solutionNumber: Int): Map[String, Any] = {
    solutionContexts.apply(solutionNumber)
  }

  def initalize(originalModelPath: String, dataPath: String, solutionsPath: String): Unit = {
    this.originalModelPath = Some(originalModelPath)
    this.dataPath = Some(dataPath)
    this.solutionsPath = Some(solutionsPath)

    val (params, vars) = ModelParser.getDataItems(this.originalModelPath.get)

    this.parameters = params
    this.variables = vars



    VarRegistry.load()
    ParameterRegistry.load()

    variables.foreach(v => VarRegistry.dataVars.contains(v.name) )

    val groupedVars: Map[ExpressionType, List[DataItem]] = (variables ++ (ParameterRegistry.dataPars.filter(_.value != None))).groupBy { item =>
      val value = item.value
      val hasValue = value != null && value != None

      if (hasValue) {
        value match {
          case _: Int | _: java.lang.Integer => IntType
          case _: Boolean | _: java.lang.Boolean => BoolType
          case _: Double | _: Float | _: java.lang.Double | _: java.lang.Float => IntType
          case l: List[_] => ListIntType
          case _ => UnknownType
        }
      } else {
        val rawType = Option(item.dataType).getOrElse("").toLowerCase
        val details = item.detailedDataType

        val isBool = (details != null && details.dataType == "bool") || rawType.contains("bool")
        val isFloat = (details != null && details.dataType == "float") || rawType.contains("float") || rawType.contains("decimal")
        val isString = (details != null && details.dataType == "string") || rawType.contains("string")

        val isArray = (details != null && details.isArray) || rawType.contains("array")
        val isSet = (details != null && details.isSet) || rawType.contains("set of")

        if (isArray || isSet) {
          if (isBool) ListAnyType
          else if (isString) ListAnyType
          else ListIntType
        } else {
          if (isBool) BoolType
          else if (isString) StringType
          else IntType
        }
      }
    }




    groupedVars.foreach { case (exprType, dataItems) =>
      debug(s"  -> Type: $exprType, Items: ${dataItems.map(_.name).mkString(", ")}")
    }

    this.variableCreatables = Some(
      groupedVars.map {
        case (exprType, dataItems) =>
        debug(s"Creating variableCreatables exprType $exprType, ${dataItems.map(_.name).mkString(", ")}")
        new RandomVariableFactory(exprType, dataItems.map(_.name))
      }.toList
    )

    this.solutionContexts = (0 until solutionCount).map(createSolutionContext).toVector
  }

  def getValue(name: String): Any = {
      val paramValue = ParameterRegistry.getValue(name)
      if (paramValue != None) {
        return paramValue
      }
      val varValue = VarRegistry.getValue(name)
      if (varValue != None) {
        if (this.variables.filter(_.name == name).head.detailedDataType.isArray) {
          val value = varValue.get.asInstanceOf[List[Any]]
          return value
        }
        else {
          return varValue.get
        }
      }
      throw new NoSuchElementException(s"No value found for '$name' in either parameters or variables for solution $solutionNumber.")
  }


  object VarRegistry extends LogTrait {
    val dataVars = {
      info(s"Parsing known solutions from: $solutionsPath...")
//      DataImporter.importDataFile(solutionsPath.getOrElse(throw new IllegalStateException("DataProvider not initialized.")), variables, true)
//      info(s"From ${variables.size} dataItems")
//      info(s"  ->${variables.filter(_.isVar).size} vars")
//      info(s"  ->${variables.count(_.value != None)} have assigned value")
//
//      variables.filter(_.value != None).foreach { d =>
//        info(s"  | Name: ${d.name}, Value count: ${d.value.asInstanceOf[List[Any]].size}, Type: ${d.dataType}, Runtime: ${d.value.asInstanceOf[List[Any]].head.getClass.getSimpleName} First value: ${d.value.asInstanceOf[List[Any]].head}")
//      }
//      variables.filter(_.value != None)


      val knownSolutions = SolutionParser.parse(solutionsPath.get, separator = ';', DataProvider.this.variables)
      if (knownSolutions.nonEmpty) {
        info("Updating variable data types based on the first found solution...")
        val firstSolutionMap = knownSolutions.head

        DataProvider.this.variables = DataProvider.this.variables.map { dataItem =>
          debug(s"DataItem is $dataItem")
          if (dataItem.detailedDataType != null) {
            dataItem
          } else {
            firstSolutionMap.get(dataItem.name) match {
              case Some(valueFromSolution) =>
                val (newDataTypeString, newDetailedType) = ModelParser.deriveDataType(valueFromSolution)
                debug(s"New typeString is: " + newDataTypeString)
                debug(s"New detailedType is: " + newDetailedType)
                dataItem.copy(
                  dataType = newDataTypeString,
                  detailedDataType = newDetailedType
                )
              case None =>
                dataItem
            }
          }
        }
        debug("Variable types have been updated.")
        DataProvider.this.variables.filter(_.isVar).foreach(v => debug(s"  -> ${v.name}: ${v.dataType}"))
      }

      if (knownSolutions.isEmpty) {
        throw new IllegalStateException("No solutions found in output file. Aborting.")
      }
      info(s"Found ${knownSolutions.size} solutions to test against.")
      DataProvider.this.solutionCount = knownSolutions.size

      info(s"Found ${knownSolutions.size} solutions to test against.")
      if (knownSolutions.isEmpty) {
        throw new IllegalStateException("No solutions found in output file. Aborting.")
      }
      knownSolutions.toVector

    }
    def load(): Unit = { return }
    def get(requiredType: ExpressionType): List[DataItem] = {
      if (variables.isEmpty) throw new IllegalStateException("DataProvider not initialized.")

      variables.filter { item =>
        val itemType = if (item.dataType == "int") IntType
        else if (item.dataType.contains("array") && item.dataType.contains("int")) ListIntType
        else if (item.dataType == "bool") BoolType
        else UnknownType
        itemType == requiredType
      }
    }

    def getValue(name: String): Option[Any] = {
      dataVars.apply(solutionNumber).get(name)
    }
  }

  object ParameterRegistry extends LogTrait {
    def load(): Unit = { return }
    val dataPars = {
      DataImporter.importDataFile(dataPath.getOrElse(throw new IllegalStateException("DataProvider not initialized.")), parameters)
      info(s"From ${parameters.size} dataItems")
      info(s"  ->${parameters.filter(!_.isVar).size} pars")
      info(s"  ->${parameters.count(_.value != None)} have assigned value")

      parameters.filter(_.value != None).foreach { d =>
        info(s"  | Name: ${d.name}, Value: ${d.value}")
      }
      parameters.filter(_.value != None)
    }

    def getAll(): List[DataItem] = {
      if (parameters.isEmpty) throw new IllegalStateException("DataProvider not initialized.")
      parameters
    }

    def getValue(name: String): Any = {
      if (dataPars.filter(_.name == name).isEmpty) return None
      dataPars.filter(_.name == name).head.value
    }
  }


  def getVariableCreatables: List[Creatable] = {
    variableCreatables.getOrElse(throw new IllegalStateException("DataProvider not initialized."))
  }
}


object ModelParser extends LogTrait {
  /* todo: figure out the correct place for this code
  val internalGrammar: ParsedGrammar = {
    val tGrammar = GrammarConverter.parseConstraintGrammar()
    debug(tGrammar.toString)
    tGrammar
  }
   */
  def getDataItems(originalModelPath: String): (List[DataItem], List[DataItem]) = {
    info(s"Reading and parsing model file $originalModelPath")
    val modelCode: String = Source.fromFile(originalModelPath).mkString
    val (_, mzparseTree) = ParserUtil.parseCode(
      code = modelCode,
      lexerFactory = (cs: CharStream) => new NewMinizincLexer(cs),
      parserFactory = (ts: TokenStream) => new NewMinizincParser(ts),
      startRuleInvoker = (p: NewMinizincParser) => p.model()
    )

    debug("Extracting all variable and parameter declarations")
    val input: CharStream = CharStreams.fromString(modelCode)
    val lexer = new NewMinizincLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val modelListener = new MinizincModelListener(tokens)

    new ParseTreeWalker().walk(modelListener, mzparseTree)
    val allDeclaredItems = modelListener.getDataItems
    debug(s"Found ${allDeclaredItems}")

    val modelDataItems = allDeclaredItems.filter(!_.isVar)
    val availableVariables = allDeclaredItems.filter(_.isVar).map(_.name)
    val availableVars = allDeclaredItems.filter(_.isVar)

    debug(s"Found ${modelDataItems.size} data items (parameters):")
    modelDataItems.foreach(item => debug(s"  -> Name: ${item.name}, Type: '${item.dataType}'"))

    debug(s"Found ${availableVariables.size} constraint variables:")
    availableVariables.foreach(item => debug(s"  -> Name: ${item}"))
    (modelDataItems,availableVars)
  }


  def deriveDataType(value: Any): (String, DataType) = {
    value match {
      case _: Int =>
        ("int", DataType(dataType = "int", isArray = false, isIdentifier = false))
      case _: Boolean =>
        ("bool", DataType(dataType = "bool", isArray = false, isIdentifier = false))
      case _: Double | _: Float =>
        ("float", DataType(dataType = "float", isArray = false, isIdentifier = false))
      case _: String =>
        ("string", DataType(dataType = "string", isArray = false, isIdentifier = false))

      case ls: List[_] if ls.nonEmpty =>
        ls.head match {
          case _: Int => ("array[int]", DataType(dataType = "int", isArray = true, isIdentifier = false))
          case _: Boolean => ("array[bool]", DataType(dataType = "bool", isArray = true, isIdentifier = false))
          case _: Double | _: Float => ("array[float]", DataType(dataType = "float", isArray = true, isIdentifier = false))
          case _: String => ("array[string]", DataType(dataType = "string", isArray = true, isIdentifier = false))
          case _ => ("array[unknown]", DataType(dataType = "unknown", isArray = true, isIdentifier = false))
        }

      case _: List[_] =>
        ("array[unknown]", DataType(dataType = "unknown", isArray = true, isIdentifier = false))

      case _ =>
        val typeName = value.getClass.getSimpleName.toLowerCase
        (typeName, DataType(dataType = typeName, isArray = false, isIdentifier = false))
    }
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
