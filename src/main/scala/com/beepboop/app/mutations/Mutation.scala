package com.beepboop.app.mutations

/* own modules */
import com.beepboop.app.components
import com.beepboop.app.components.{BoolType, ComponentRegistry, Constant, DiffnExpression, Expression, ListIntType, OperatorContainer, RandomVariableFactory, Variable}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.LogTrait

trait Mutation extends LogTrait {
  def name: String
  def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]]
}


object GenerateAllDiffn extends Mutation {
  override def name: String = "GenerateAllDiffn"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    if (expression.signature.output != BoolType) return Nil

    val allItems = (DataProvider.variables ++ DataProvider.ParameterRegistry.getAll()).distinct

    val arrayCandidates = allItems.filter { item =>
      item.detailedDataType != null && item.detailedDataType.isArray
    }.map(_.name).distinct

    if (arrayCandidates.size < 4) {
      return Nil
    }

    arrayCandidates.combinations(4).flatMap(_.permutations).map { case List(x, y, dx, dy) =>
      DiffnExpression(
        Variable[List[Integer]](x),
        Variable[List[Integer]](y),
        Variable[List[Integer]](dx),
        Variable[List[Integer]](dy)
      )
    }.toList.distinct
  }
}


object ReplaceOperator extends Mutation {
  override def name: String = "ReplaceOperator"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case container: OperatorContainer =>
        val compatibleOps = ComponentRegistry
          .findOperatorsWithSignature(container.operator.signature)
          .filter(_.toString != container.operator.toString)

        if (compatibleOps.nonEmpty) {
          val newOp = compatibleOps(scala.util.Random.nextInt(compatibleOps.length))
          List(container.withNewOperator(newOp))
        } else {
          Nil
        }
      case _ => Nil
    }
  }
}


object TransformVariableToConstant extends Mutation {
  override def name: String = "TransformVariableToConstant"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case v: Variable[?] =>
        val requiredType = v.signature.output
        val constantFactory = ComponentRegistry.findCreatablesReturning(requiredType)
          .find(_.templateSignature.inputs.isEmpty)

        constantFactory.map(_.create(Nil)).toList

      case _ => Nil
    }
  }
}


object TransformConstantToVariable extends Mutation {
  override def name: String = "TransformConstantToVariable"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case c: Constant[?] =>
        val requiredType = c.signature.output
        val possibleFactories = ComponentRegistry.findCreatablesReturning(requiredType)
          .filter(_.templateSignature.inputs.isEmpty)

        val variableFactory = possibleFactories.collectFirst {
          case factory: RandomVariableFactory => factory
        }
        variableFactory.map(_.create(Nil)).toList
      case _ =>
        Nil
    }
  }
}

object ChangeVariable extends Mutation {
  override def name: String = "ChangeVariable"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case v: Variable[?] =>
        val requiredType = v.signature.output
        val currentName = v.name

        val variableFactory = ComponentRegistry.creatables.collectFirst {
          case factory: RandomVariableFactory if factory.varType == requiredType => factory
        }

        variableFactory.flatMap { factory =>
          val alternativeNames = factory.availableNames.filter(_ != currentName)
          if (alternativeNames.nonEmpty) {
            val newName = alternativeNames(scala.util.Random.nextInt(alternativeNames.length))
            Some(factory.createWithName(newName))
          } else {
            None
          }
        }.toList
      case _ => Nil
    }
  }
}

class ReplaceSubtree(val maxDepth: Int) extends Mutation {
  override def name: String = "ReplaceSubtree"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    val requiredType = expression.signature.output
    val generated = ExpressionGenerator.generate(requiredType, maxDepth = maxDepth, ctx)
    generated.toList
  }
}