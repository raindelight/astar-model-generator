package com.beepboop.app.experiment

import com.beepboop.app.components.Expression


trait TargetModel {
  def targetConstraints: List[Expression[?]]
  def similarityPolicy: SimilarityPolicy = SimilarityPolicy()
}

case class SimilarityPolicy(
                             swappableOperators: Set[String] = Set("AddOperator", "MultiplyOperator", "AndOperator", "OrOperator"),
                             ignoredVariables: Set[String] = Set.empty,
                             ignoreVariableNames: Boolean = false
                           )
