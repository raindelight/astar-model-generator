package com.beepboop.app.mutations

/* own modules */
import com.beepboop.app.components
import com.beepboop.app.components.{ComponentRegistry, Constant, Expression, OperatorContainer, RandomVariableFactory, Variable}
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.LogTrait

trait Mutation extends LogTrait {
  def name: String
  def apply(expression: Expression[?]): Option[Expression[?]]
}



object ReplaceOperator extends Mutation {
  override def name: String = "ReplaceOperator"

  override def apply(expression: Expression[?]): Option[Expression[?]] = {
    expression match {
      case container: OperatorContainer =>
        val compatibleOps = ComponentRegistry
          .findOperatorsWithSignature(container.operator.signature)
          .filter( cpo => 
            { debug(s"${cpo.toString} != ${container.operator.toString}")
            cpo.toString != container.operator.toString}) // todo: testing with strings
        debug(s"All available operators for signature ${container.operator.signature} are ${compatibleOps.map(_.toString)}")

        if (compatibleOps.nonEmpty) {
          val newOp = compatibleOps(scala.util.Random.nextInt(compatibleOps.length))
          Some(container.withNewOperator(newOp))
        } else {
          None
        }
      case _ => None
    }
  }
}

object TransformVariableToConstant extends Mutation {
  override def name: String = "TransformVariableToConstant"

  override def apply(expression: Expression[?]): Option[Expression[?]] = {
    expression match {
      case v: Variable[?] =>
        val requiredType = v.signature.output
        val constantFactory = ComponentRegistry.findCreatablesReturning(requiredType)
          .find(_.templateSignature.inputs.isEmpty)

        constantFactory.map(_.create(Nil))

      case _ => None
    }
  }
}

object TransformConstantToVariable extends Mutation {
  override def name: String = "TransformVariableToConstant"


  override def apply(expression: Expression[?]): Option[Expression[?]] = {
    expression match {
      case c: Constant[?] =>

        val requiredType = c.signature.output

        val possibleFactories = ComponentRegistry.findCreatablesReturning(requiredType)
          .filter(_.templateSignature.inputs.isEmpty)

        val variableFactory = possibleFactories.collectFirst {
          case factory: RandomVariableFactory => factory
        }
        variableFactory.map(_.create(Nil))
      case _ =>
        None
    }
  }
}


object ChangeVariable extends Mutation {
  override def name: String = "ChangeVariable"

  override def apply(expression: Expression[?]): Option[Expression[?]] = {
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
        }
      case _ => None
    }
  }
}


class ReplaceSubtree(val maxDepth: Int) extends Mutation {
  override def name: String = "ReplaceSubtree"

  override def apply(expression: Expression[?]): Option[Expression[?]] = {
    val requiredType = expression.signature.output

    val generated = ExpressionGenerator.generate(requiredType, maxDepth = maxDepth)
    debug(s"generated expressions are: $generated")
    generated


  }
}