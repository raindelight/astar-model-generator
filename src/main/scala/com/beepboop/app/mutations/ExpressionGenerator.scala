package com.beepboop.app.mutations
/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.utils.AppConfig
import com.beepboop.app.logger.LogTrait


case class GenerationContext(
                              variables: Map[ExpressionType, List[String]] = Map.empty,
                            ) {
  def withVariable(name: String, varType: ExpressionType, domain: Expression[?]): GenerationContext = {
    val existingNames = variables.getOrElse(varType, List.empty)
    copy(
      variables = variables + (varType -> (name :: existingNames)),
    )
  }
}
object ExpressionGenerator extends LogTrait {

  def generate(
                requiredType: ExpressionType,
                maxDepth: Int,
                ctx: GenerationContext = GenerationContext(),
                exclude: Option[Class[_]] = None
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

    val filteredPool = exclude match {
      case Some(cls) =>
        val excludeName = cls.getSimpleName.stripSuffix("$")
        possibleCreatables.filterNot(_.toString == excludeName)
      case None =>
        possibleCreatables
    }
    //println(exclude.toString)
    //println(filteredPool.mkString(","))


    //val finalPool = if (filteredPool.nonEmpty) filteredPool else possibleCreatables


    val chosenCreatable = selectWeighted(possibleCreatables, ctx)


    debug(s"Selected: ${chosenCreatable.getClass.getName}")



    val result = chosenCreatable match {
      case scoped: ContextAwareCreatable =>
        scoped.generateExpression(ctx, (t, c) => generate(t, maxDepth - 1, c))

      case standard =>
        val inputs = standard.templateSignature.inputs
        val children = inputs.map(inputType => generate(inputType, maxDepth - 1, ctx))
        if (children.forall(_.isDefined)) {
          val flattenedChildren = children.flatten
          val actualChildTypes = flattenedChildren.map(_.signature.output)
          if (actualChildTypes == inputs) {
            Some(standard.create(flattenedChildren))
          } else {
            debug(s"Type mismatch during standard creation: Expected $inputs but got $actualChildTypes")
            None
          }
        } else {
          None
        }
    }
    result.foreach { expr =>
      expr.creatorInfo = s"Created by [${chosenCreatable.toString}] at Depth [$maxDepth] for Type [$requiredType] - ${result.get.toString}. "
    }
    result
  }

  private def selectWeighted(candidates: List[Creatable], ctx: GenerationContext): Creatable = {
    val candidatesWithWeights = candidates.map { c =>
      val name = c.toString
      val weight = c match {
        case factory: RandomVariableFactory =>
          val localNames = ctx.variables.getOrElse(factory.varType, Nil)

          if (factory.availableNames.exists(localNames.contains)) {
            800.0
          } else {
            50.0
          }
        case _ =>
          AppConfig.getWeight(name)
      }
      (c, weight)
    }
    val totalWeight = candidatesWithWeights.map(_._2).sum

    if (totalWeight <= 0) {
      candidates(scala.util.Random.nextInt(candidates.length))
    } else {
      val randomValue = scala.util.Random.nextDouble() * totalWeight

      val result = candidatesWithWeights.scanLeft((0.0, None: Option[Creatable])) {
        case ((acc, _), (creatable, weight)) => (acc + weight, Some(creatable))
      }.find { case (cumulative, _) => cumulative > randomValue }

      result.flatMap(_._2).getOrElse(candidates.last)
    }
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
