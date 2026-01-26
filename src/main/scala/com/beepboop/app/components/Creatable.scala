package com.beepboop.app.components

import com.beepboop.app.logger.LogTrait


class RandomVariableFactory(
                             val varType: ExpressionType,
                             val availableNames: List[String]
                           ) extends Creatable   with AutoNamed  with LogTrait {
  override def templateSignature: Signature = Signature(inputs = Nil, output = varType)

  override def create(children: List[Expression[?]]): Expression[?] = {
    require(children.isEmpty)
    if (availableNames.isEmpty) {
      throw new IllegalStateException(s"No variables of type $varType are available.")
    }
    debug(availableNames.mkString(","))
    val randomName = availableNames(scala.util.Random.nextInt(availableNames.length))

    createWithName(randomName)
  }

  override def ownerClass: Class[_] = Variable.getClass

  def createWithName(name: String): Expression[?] = {
    varType match {
      case IntType => Variable[Integer](name)
      case BoolType => Variable[Boolean](name)
      case ListIntType => Variable[List[Integer]](name)
      case ListListIntType => Variable[List[List[Integer]]](name)
    }
  }
}