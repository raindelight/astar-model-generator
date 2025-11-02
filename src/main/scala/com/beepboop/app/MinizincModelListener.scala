package com.beepboop.app

//import com.beepboop.parser.NewMinizincBaseListener
// com.beepboop.parser.NewMinizincParser.ParDeclItemContext
import com.beepboop.parser.NewMinizincParserBaseListener
import com.beepboop.parser.NewMinizincParser.Var_decl_itemContext
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import com.beepboop.app.dataprovider.*

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class MinizincModelListener(tokens: CommonTokenStream, extendedDataType: Boolean = true) extends NewMinizincParserBaseListener {

  private val dataItemsBuffer = new ListBuffer[DataItem]()

  def getDataItems: List[DataItem] = dataItemsBuffer.toList

  private def textWithSpaces(ctx: ParserRuleContext): String = {
    val start = ctx.getStart.getTokenIndex
    val stop = ctx.getStop.getTokenIndex
    tokens.getText(new Interval(start, stop))
  }

  override def enterVar_decl_item(ctx: Var_decl_itemContext): Unit = {
    assert(ctx != null)
    var name =
      if (ctx.ident() == null) ctx.ti_expr_and_id().ident().getText
      else ctx.ident().getText

    var fullType = ""
    var detailedFullType = DataType(null, false, false)

    var isVar = false

    if (ctx.ti_expr_and_id().ti_expr().array_ti_expr() != null) {
     if (ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().var_par().getText != "var")
     {
       fullType = textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr())
       if (ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().base_type() == null) {
         detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().ident()), true, true)
       } else {
         detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().base_type()), true, false)
       }
     } else {
       fullType = textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr())
       if (ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().base_type() == null) {
         if (ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().ident() != null) {
           detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().ident()), true, true)
         } else {
           detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr()), true, true)
         }
       } else {
         detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().array_ti_expr().base_ti_expr().base_ti_expr_tail().base_type()), true, false)
       }
       isVar = true
     }

    } else if (ctx.ti_expr_and_id().ti_expr().base_ti_expr() != null)  {
        if (ctx.ti_expr_and_id().ti_expr().base_ti_expr().var_par().getText != "var") {
          fullType = textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr())
          detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().base_type()), false, false)
        } else {
          fullType = textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr())
          if (ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().base_type() == null){
            if (ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().ident() != null){
              detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().ident()), false, true)
            } else {
              detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr()), false, true)
            }
          } else {
            detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().base_type()), false, false)
          }
          isVar = true
        }
    }

    val dataItem = if(extendedDataType){
      DataItem(name = name, dataType = fullType, isVar = isVar, detailedDataType = detailedFullType)
    } else {
     DataItem(name = name, dataType = fullType, isVar = isVar, detailedDataType = null)
    }
    dataItemsBuffer += dataItem
  }
}