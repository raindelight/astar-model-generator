package com.beepboop.app.mutations
/* own modules */
import com.beepboop.app.components.*
import com.beepboop.app.logger.LogTrait

object ExpressionGenerator extends LogTrait {
  def generate(requiredType: ExpressionType, maxDepth: Int): Option[Expression[?]] = {
    val registry = ComponentRegistry
    val possibleCreatables = if (maxDepth <= 0) {
      registry.findCreatablesReturning(requiredType).filter(_.templateSignature.inputs.isEmpty)
    } else {
      registry.findCreatablesReturning(requiredType)
    }
    debug(s"possibleCreatables count: ${possibleCreatables.length}")

    if (possibleCreatables.isEmpty) return {
      warn(s"possibleCreatables are empty, returning")
      None
    }

    val chosenCreatable = possibleCreatables(scala.util.Random.nextInt(possibleCreatables.length))
    debug(s"chosenCreatable is: ${chosenCreatable.toString}")
    val requiredInputs = chosenCreatable.templateSignature.inputs
    debug(s"requiredInputs for $chosenCreatable are: $requiredInputs")
    val result = sequence(requiredInputs.map(inputType => {
      val generated = generate(inputType, maxDepth - 1)
      debug(s"we pass required input type $inputType, and we generated ${generated.toString} [${maxDepth - 1}]")
      generated
    }))
      .map(children => chosenCreatable.create(children))

    result
  }

  private def sequence[T](opts: List[Option[T]]): Option[List[T]] =
    opts.foldRight[Option[List[T]]] (Some(Nil)) { (opt, acc) =>
      for (x <- opt; xs <- acc) yield x :: xs

    }
}