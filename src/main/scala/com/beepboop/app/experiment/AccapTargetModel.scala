package com.beepboop.app.experiment

import com.beepboop.app.components.*
import com.beepboop.app.utils.Implicits.integerNumeric

class AccapTargetModel extends TargetModel {

  override val similarityPolicy = SimilarityPolicy(
    swappableOperators = Set("AddOperator"),
  )

  override def targetConstraints: List[Expression[_]] = List(
    DiffnExpression(
      Variable("xCoor"),
      Variable("yCoor"),
      Variable("opDur"),
      Variable("cNum")
    ),
    ForAllExpression(
      IteratorDef[List[Integer]]("f", Variable("FLIGHT")),
      BinaryExpression(
        BinaryExpression(
          BinaryExpression(
            ArrayElement[Integer](Variable("yCoor"), Variable("f")),
            AddOperator[Integer](),
            ArrayElement[Integer](Variable("cNum"), Variable("f"))
          ),
          SubOperator[Integer](),
          Constant(1)
        ),
        LessEqualOperator[Integer](),
        Variable[Integer]("D")
      )
    ),
    BinaryExpression(
      SumExpression[Integer](
        Variable("S")
      ),
      AddOperator[Integer](),
      Variable[Integer]("D")
    )
  )
}