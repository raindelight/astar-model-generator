package com.beepboop.app.mutations

/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.logger.LogTrait

object AllMutations {
  val mutations: List[Mutation] = List(
    //ReplaceOperator,
    //TransformVariableToConstant,
    //TransformConstantToVariable,
    //ChangeVariable,
    ReplaceSubtree(1),
    //ReplaceSubtree(2),
    //ReplaceSubtree(3),
    //ReplaceSubtree(4),
    GenerateAllDiffn
  )

  val directory: Map[String, Mutation] = mutations.map(m => m.name -> m).toMap
}


class MutationEngine(val activeMutations: List[Mutation]) extends LogTrait {
  def mutate(tree: Expression[?]): Expression[?] = {
    val allPossibleMutations = collectPossibleMutations(tree)

    if (allPossibleMutations.isEmpty) {
      warn("No possible mutations found")
      tree
    } else {
      val (nodeToReplace, mutationToApply, contextForNode, selectedVariant) = allPossibleMutations(
        scala.util.Random.nextInt(allPossibleMutations.length)
      )

      info(s"Applying mutation '${mutationToApply.name}' to node '$nodeToReplace'")

      replaceNodeInTree(tree, nodeToReplace, selectedVariant)
    }
  }

  def collectPossibleMutations(
                                current: Expression[?],
                                ctx: GenerationContext = GenerationContext()
                              ): List[(Expression[?], Mutation, GenerationContext, Expression[?])] = {

    val mutationsForThisNode = activeMutations.flatMap { mutation =>
      val variants = mutation(current, ctx)

      variants.map { variant =>
        (current, mutation, ctx, variant)
      }
    }

    debug(s"Mutations available for node ${current.toString}: ${mutationsForThisNode.map(_._2.name)}")

    val mutationsForChildren = current match {
      case f: ForAllExpression[?] =>
        val listType = f.iteratorDef.collection.signature.output
        val innerType = listType match {
          case ListIntType => IntType
          case _ => UnknownType
        }

        val innerCtx = ctx.withVariable(f.iteratorDef.variableName, innerType)

        collectPossibleMutations(f.iteratorDef.collection, ctx) ++
          collectPossibleMutations(f.body, innerCtx)

      case c: ComposableExpression =>
        c.children.flatMap(child => collectPossibleMutations(child, ctx))

      case _ =>
        List.empty
    }

    mutationsForThisNode ++ mutationsForChildren
  }

  def replaceNodeInTree(root: Expression[?], target: Expression[?], replacement: Expression[?]): Expression[?] = {
    if (root eq target) {
      return replacement
    }

    root match {
      case c: ComposableExpression =>
        val newChildren = c.children.map(child => replaceNodeInTree(child, target, replacement))
        val childrenChanged = newChildren.zip(c.children).exists { case (newChild, oldChild) => !(newChild eq oldChild) }

        if (childrenChanged) {
          c.withNewChildren(newChildren)
        } else {
          c
        }
      case other => other
    }
  }
}