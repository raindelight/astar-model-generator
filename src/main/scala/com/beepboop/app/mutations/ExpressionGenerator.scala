package com.beepboop.app.mutations
/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.logger.LogTrait


case class GenerationContext(
                              variables: Map[ExpressionType, List[String]] = Map.empty
                            ) {
  def withVariable(name: String, varType: ExpressionType): GenerationContext = {
    val existingNames = variables.getOrElse(varType, List.empty)
    copy(variables = variables + (varType -> (name :: existingNames)))
  }
}

object ExpressionGenerator extends LogTrait {

  def generate(
                requiredType: ExpressionType,
                maxDepth: Int,
                ctx: GenerationContext = GenerationContext()
              ): Option[Expression[?]] = {

    val registry = ComponentRegistry

    val globalCreatables = if (maxDepth <= 0) {
      registry.findCreatablesReturning(requiredType).filter(_.templateSignature.inputs.isEmpty)
    } else {
      registry.findCreatablesReturning(requiredType)
    }

    val localNames = ctx.variables.getOrElse(requiredType, Nil)
    val localCreatables = if (localNames.nonEmpty) {
      List(new RandomVariableFactory(requiredType, localNames))
    } else {
      Nil
    }

    val possibleCreatables = globalCreatables ++ localCreatables
    debug(s"--- Selection Pool [Depth: $maxDepth, Type: $requiredType] ---")
    possibleCreatables.foreach { c =>
      val kind = c match {
        case _: RandomVariableFactory => "[LOCAL VAR]"
        case _: ContextAwareCreatable => "[SCOPED OP]"
        case x if x.templateSignature.inputs.isEmpty => "[CONSTANT ]"
        case _ => "[OPERATOR ]"
      }
      debug(s"$kind ${c.getClass.getName} -> Returns: ${c.templateSignature.output}")
    }
    debug(s"-------------------------------------------------------------")
    if (possibleCreatables.isEmpty) {

      return None
    }
    val chosenCreatable = selectWeighted(possibleCreatables)


    debug(s"Selected: ${chosenCreatable.getClass.getName}")



    chosenCreatable match {
      case scoped: ContextAwareCreatable =>
        scoped.generateExpression(ctx, (t, c) => generate(t, maxDepth - 1, c))

      case standard =>
        val inputs = standard.templateSignature.inputs
        val children = inputs.map(inputType => generate(inputType, maxDepth - 1, ctx)) // Pass same ctx

        if (children.forall(_.isDefined)) {
          Some(standard.create(children.flatten))
        } else {
          None
        }
    }
  }

  private def selectWeighted(candidates: List[Creatable]): Creatable = {
    val registry = ComponentRegistry

    val candidatesWithWeights = candidates.map { c =>
      val weight = c match {
        case _: RandomVariableFactory => 50.0
        case _ => registry.getWeight(c)
      }
      (c, weight)
    }

    val totalWeight = candidatesWithWeights.map(_._2).sum

    if (totalWeight <= 0) return candidates(scala.util.Random.nextInt(candidates.length))

    val randomValue = scala.util.Random.nextDouble() * totalWeight

    var cumulativeWeight = 0.0
    for ((creatable, weight) <- candidatesWithWeights) {
      cumulativeWeight += weight
      if (randomValue < cumulativeWeight) {
        return creatable
      }
    }

    candidates.last
  }

  private def sequence[T](opts: List[Option[T]]): Option[List[T]] =
    opts.foldRight[Option[List[T]]] (Some(Nil)) { (opt, acc) =>
      for (x <- opt; xs <- acc) yield x :: xs

    }
}


trait ContextAwareCreatable extends Creatable {
  def generateExpression(
                          ctx: GenerationContext,
                          recurse: (ExpressionType, GenerationContext) => Option[Expression[?]]
                        ): Option[Expression[?]]
}