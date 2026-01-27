package com.beepboop.app.components

import com.beepboop.app.logger.LogTrait


class RandomVariableFactory(
                             val varType: ExpressionType,
                             val availableNames: List[String]
                           ) extends Creatable with AutoNamed {

  override def templateSignature: Signature = Signature(inputs = Nil, output = varType)

  override def ownerClass: Class[_] = classOf[Variable[_]]

  override def create(children: List[Expression[?]]): Expression[?] = {
    if (availableNames.isEmpty) throw new IllegalStateException(s"No variables found for type $varType")
    val randomName = availableNames(scala.util.Random.nextInt(availableNames.size))
    createWithName(randomName)
  }

  def createWithName(name: String): Expression[?] = {
    varType match {
      case IntType => Variable[Integer](name, explicitType = Some(IntType))
      case BoolType => Variable[Boolean](name, explicitType = Some(BoolType))
      case StringType => Variable[String](name, explicitType = Some(StringType))

      case ListIntType => Variable[List[Integer]](name, explicitType = Some(ListIntType))
      case ListBoolType => Variable[List[Boolean]](name, explicitType = Some(ListBoolType))
      case ListListIntType => Variable[List[List[Integer]]](name, explicitType = Some(ListListIntType))
      case SetIntType => Variable[Set[Integer]](name, explicitType = Some(SetIntType))
      case ListSetIntType => Variable[List[Set[Integer]]](name, explicitType = Some(ListSetIntType))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported variable type factory for: $varType")
    }
  }
}