package com.beepboop.app.experiment

import com.beepboop.app.components.*
import com.beepboop.app.utils.Implicits.integerNumeric

class AccapTargetModel extends TargetModel {

  override val similarityPolicy = SimilarityPolicy(
    swappableOperators = Set("AddOperator"),
  )

  override def targetConstraints: List[Expression[_]] = List(
    DiffnExpression(
      Variable[List[Integer]]("xCoor"),
      Variable[List[Integer]]("yCoor"),
      Variable[List[Integer]]("opDur"),
      Variable[List[Integer]]("cNum")
    ),

    // C2: Height Limit Constraint
    ForAllExpression(
      IteratorDef("f", Variable[List[Integer]]("FLIGHT")),
      BinaryExpression(
        BinaryExpression(
          BinaryExpression(
            ArrayElement[Integer](Variable[List[Integer]]("yCoor"), Variable[Integer]("f")),
            AddOperator[Integer](),
            ArrayElement[Integer](Variable[List[Integer]]("cNum"), Variable[Integer]("f"))
          ),
          SubOperator[Integer](),
          Constant[Integer](1)
        ),
        LessEqualOperator[Integer](),
        Variable[Integer]("D")
      )
    ),

    // C3: Distance Constraint (The missing piece) [cite: 587-591]
    ForAllExpression(
      // Outer Loop: a in AIRLINE
      IteratorDef("a", Variable[List[Integer]]("AIRLINE")),
      ForAllExpression(
        // Middle Loop: f in FA[a]
        IteratorDef("f", ArrayElement(Variable[List[List[Integer]]]("FA"), Variable[Integer]("a"))),
        ForAllExpression(
          // Inner Loop: g in FA[a]
          IteratorDef("g", ArrayElement(Variable[List[List[Integer]]]("FA"), Variable[Integer]("a"))),
          BinaryExpression(
            // Filter: f != g
            BinaryExpression(
              Variable[Integer]("f"),
              NotEqualOperator[Integer](),
              Variable[Integer]("g")
            ),
            ImpliesOperator[Boolean](),
            // Body: ((yCoor[f] + cNum[f] - 1) - yCoor[g]) <= S[a]
            BinaryExpression(
              BinaryExpression(
                BinaryExpression(
                  BinaryExpression(
                    ArrayElement(Variable[List[Integer]]("yCoor"), Variable[Integer]("f")),
                    AddOperator[Integer](),
                    ArrayElement(Variable[List[Integer]]("cNum"), Variable[Integer]("f"))
                  ),
                  SubOperator[Integer](),
                  Constant[Integer](1)
                ),
                SubOperator[Integer](),
                ArrayElement(Variable[List[Integer]]("yCoor"), Variable[Integer]("g"))
              ),
              LessEqualOperator[Integer](),
              ArrayElement(Variable[List[Integer]]("S"), Variable[Integer]("a"))
            )
          )
        )
      )
    ),

    BinaryExpression(
      Variable[Integer]("objective"),
      EqualOperator[Integer](),
      BinaryExpression(
        SumExpression[Integer](
          Variable[List[Integer]]("S")
        ),
        AddOperator[Integer](),
        Variable[Integer]("D")
      )
    )
  )
}
