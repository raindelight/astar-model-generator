package com.beepboop.app.experiment

import com.beepboop.app.components.*
import com.beepboop.app.utils.Implicits.integerNumeric

class ConcertHallTargetModel extends TargetModel {

  override val similarityPolicy = SimilarityPolicy(
    swappableOperators = Set("+", "*", "and", "or", "=", "!=", "diffn", "alldifferent_except_0", "contains")
  )

  override def targetConstraints: List[Expression[?]] = List(
    // -------------------------------------------------------
    // C1: Overlap Constraint (Clique Constraints)
    // constraint forall (clique in cliques ...) (alldifferent_except_0([assign[o] | o in clique]));
    // -------------------------------------------------------
    ForAllExpression(
      // Iterate directly over the precomputed cliques structure
      // cliques is likely List[List[Int]] or List[Set[Int]] in the data
      IteratorDef("clique", Variable[List[Integer]]("cliques")),
      AllDifferentExceptZeroExpression(
        SetComprehensionExpression(
          // Head: assign[p]
          ArrayElement(Variable[List[Integer]]("assign"), Variable[Integer]("p")),
          // Iterator: p in current clique
          IteratorDef("p", Variable[List[Integer]]("clique")),
          // Filter: true (take all elements in the clique)
          Constant[Boolean](true)
        )
      )
    ),

    // -------------------------------------------------------
    // C2: Capacity Constraint (Feasibility)
    // forall (o in Offer) (assign[o] in {0} union feasible_halls(o));
    // -------------------------------------------------------
    ForAllExpression(
      IteratorDef("o", Variable[List[Integer]]("Offer")),
      BinaryExpression(
        // Option A: assign[o] is 0 (unassigned)
        BinaryExpression(
          ArrayElement(Variable[List[Integer]]("assign"), Variable[Integer]("o")),
          EqualOperator[Integer](),
          Constant[Integer](0)
        ),
        OrOperator[Boolean](),
        // Option B: assign[o] is in feasible_halls[o]
        // We use ContainsOperator on the precomputed set
        BinaryExpression(
          ArrayElement(Variable[List[Integer]]("feasible_halls"), Variable[Integer]("o")),
          ContainsOperator[List[Integer], Integer](),
          ArrayElement(Variable[List[Integer]]("assign"), Variable[Integer]("o"))
        )
      )
    ),

    // -------------------------------------------------------
    // C3: Objective Function
    // objective = sum (o in Offer) (price[o] * (assign[o] > 0));
    // -------------------------------------------------------
    BinaryExpression(
      Variable[Integer]("objective"),
      EqualOperator[Integer](),
      SumExpression(
        SetComprehensionExpression(
          // Head: price[o] * bool2int(assign[o] > 0)
          BinaryExpression(
            ArrayElement(Variable[List[Integer]]("price"), Variable[Integer]("o")),
            MulOperator[Integer](),
            UnaryExpression(
              BinaryExpression(
                ArrayElement(Variable[List[Integer]]("assign"), Variable[Integer]("o")),
                GreaterOperator[Integer](),
                Constant[Integer](0)
              ),
              BoolToIntOperator[Boolean]()
            )
          ),
          // Iterator: o in Offer
          IteratorDef("o", Variable[List[Integer]]("Offer")),
          Constant[Boolean](true)
        )
      )
    ),

    // -------------------------------------------------------
    // Optional C4: Symmetry Breaking (if required)
    // forall (h in Hall) (value_precede_chain(eqclass(h), assign))
    // -------------------------------------------------------
    ForAllExpression(
        IteratorDef("h", Variable[List[Integer]]("Hall")),
        ValuePrecedesChainExpression(
           Variable[List[Integer]]("assign"),
           ArrayElement(Variable[List[List[Integer]]]("eqclass"), Variable[Integer]("h"))
        )
    )
  )
}