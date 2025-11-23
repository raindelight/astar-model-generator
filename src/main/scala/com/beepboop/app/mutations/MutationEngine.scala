package com.beepboop.app.mutations

/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.logger.LogTrait

object AllMutations {
  val mutations: List[Mutation] = List(
    ReplaceOperator,
    TransformVariableToConstant,
    TransformConstantToVariable,
    ChangeVariable,
    ReplaceSubtree(1),
    ReplaceSubtree(2),
  )

  val directory: Map[String, Mutation] = mutations.map(m => m.name -> m).toMap
}


class MutationEngine(val activeMutations: List[Mutation]) extends LogTrait {
  def mutate(tree: Expression[?]): Expression[?]= {
    val allPossibleMutations = collectPossibleMutations(tree)

    if (allPossibleMutations.isEmpty) {
      warn("No possible mutations found")
      tree
    } else {
      val (nodeToReplace, mutationToApply) = allPossibleMutations(
        scala.util.Random.nextInt(allPossibleMutations.length)
      )

      info(s"Applying mutation '${mutationToApply.name} to node '${nodeToReplace}'")

      replaceNodeInTree(tree, nodeToReplace, mutationToApply(nodeToReplace).get)

    }
  }


  def collectPossibleMutations(
                                      current: Expression[?]
                                      ): List[(Expression[?], Mutation)] = {
    val mutationsForThisNode = activeMutations
      .filter(mutation => mutation(current).isDefined)
      .map(mutation => (current, mutation))
    debug(s"Mutations available for node ${current.toString}: ${mutationsForThisNode.map(mut => mut._2.name)}")
    val mutationsForChildren = current match {
      case c: ComposableExpression =>
        c.children.flatMap(child => collectPossibleMutations(child))
      case _ =>
        List.empty
    }
    mutationsForThisNode ++ mutationsForChildren
  }

   def replaceNodeInTree(root: Expression[?], target: Expression[?], replacement: Expression[?]): Expression[?] = {
    if (root eq target){
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