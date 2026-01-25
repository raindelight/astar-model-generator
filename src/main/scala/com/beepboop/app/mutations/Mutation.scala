package com.beepboop.app.mutations

import com.beepboop.app.components.{ArrayElement, ComposableExpression, IntType}
import pureconfig.ConfigReader

import scala.reflect.ClassTag

/* own modules */
import com.beepboop.app.components
import com.beepboop.app.components.{BoolType, ComponentRegistry, Constant, DiffnExpression, Expression, ListIntType, OperatorContainer, RandomVariableFactory, Variable}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.LogTrait


sealed trait Mutation extends LogTrait derives ConfigReader {
  def enabled: Boolean
  def name: String
  def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]]
}


case class GenerateAllDiffn(enabled: Boolean = true) extends Mutation {
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


case class ReplaceOperator(enabled: Boolean = true) extends Mutation {
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


case class TransformVariableToConstant(enabled: Boolean = true) extends Mutation {
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


case class TransformConstantToVariable(enabled: Boolean = true) extends Mutation {
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

case class ChangeVariable(enabled: Boolean = true) extends Mutation {
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

case class ReplaceSubtree(val maxDepth: Int, enabled: Boolean = true) extends Mutation {
  override def name: String = "ReplaceSubtree"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    val requiredType = expression.signature.output
    val generated = ExpressionGenerator.generate(requiredType, maxDepth = maxDepth, ctx, Some(expression.getClass))
    generated.toList
  }
}

case class IteratorCoupling(enabled: Boolean = true) extends Mutation {
  override def name: String = "IteratorCoupling"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case ae: ArrayElement[t] =>
        implicit val tag: ClassTag[t] = ae.ct
        val localInts = ctx.variables.getOrElse(IntType, Nil)

        localInts.map { varName =>
          ae.copy(index = Variable[Integer](varName))
        }
      case _ => Nil
    }
  }
}

case class UnwrapExpression(enabled: Boolean = true) extends Mutation {
  override def name: String = "UnwrapExpression"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case c: ComposableExpression =>
        val compatibleChildren = c.children.filter(_.signature.output == expression.signature.output)

        compatibleChildren
      case _ => Nil
    }
  }
}


case class PruneToLeaf(enabled: Boolean = true) extends Mutation {
  override def name: String = "PruneToLeaf"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case c: ComposableExpression if c.children.nonEmpty =>
        val requiredType = expression.signature.output

        val simpleReplacements = ComponentRegistry.findCreatablesReturning(requiredType)
          .filter(_.templateSignature.inputs.isEmpty)
          .flatMap { factory =>
            factory match {
              case v: RandomVariableFactory =>
                v.availableNames.map(n => v.createWithName(n))
              case f => Some(f.create(Nil))
            }
          }

        scala.util.Random.shuffle(simpleReplacements).take(3).toList

      case _ => Nil
    }
  }
}


case class InjectQuantifier(enabled: Boolean = true) extends Mutation {
  override def name: String = "InjectQuantifier"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    if (expression.signature.output != components.BoolType) return Nil

    val factories = List(
      components.ForAllExpression.ForAllIntListFactory,
      //components.ExistsExpression.ExistsIntListFactory
    )

    factories.flatMap { factory =>
      factory.generateExpression(ctx, (t, c) =>
        ExpressionGenerator.generate(t, maxDepth = 3, c)
      )
    }
  }
}

case class InjectNestedLoop(enabled: Boolean = true) extends Mutation {
  override def name: String = "InjectNestedLoop"

  override def apply(expression: Expression[?], ctx: GenerationContext): List[Expression[?]] = {
    expression match {
      case outerLoop: components.ForAllExpression[_] =>
        val outerIter = outerLoop.iteratorDef.variableName
        val outerType = outerLoop.iteratorDef.collection.signature.output

        val candidateCollections = DataProvider.variables.filter { v =>
          v.detailedDataType.isArray &&
            v.detailedDataType.dataType == "int"
        }

        if (candidateCollections.isEmpty) return Nil

        val targetCollName = candidateCollections(scala.util.Random.nextInt(candidateCollections.size)).name

        val dependentCollection = components.ArrayElement[List[Integer]](
          components.Variable[List[List[Integer]]](targetCollName),
          components.Variable[Integer](outerIter)                   
        )

        val innerIterName = com.beepboop.app.dataprovider.VarNameGenerator.generateUniqueName("inner")
        val innerIterator = components.IteratorDef(innerIterName, dependentCollection)

        val newInnerLoop = components.ForAllExpression(innerIterator, outerLoop.body)

        List(components.ForAllExpression(outerLoop.iteratorDef, newInnerLoop))

      case _ => Nil
    }
  }
}