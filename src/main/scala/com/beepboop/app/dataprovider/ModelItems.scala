package com.beepboop.app.dataprovider

sealed trait ModelItem {
  def name: String
}

case class DataType(
                   dataType: String,
                   isArray: Boolean,
                   isIdentifier: Boolean, // todo: isSet?
                   isSet: Boolean = false,
                   )

case class DataItem(
                     override val name: String,
                     dataType: String,
                     isVar: Boolean,
                     var value: Any = None,
                     detailedDataType: DataType = null,
                     expr: String,
                   ) extends ModelItem
