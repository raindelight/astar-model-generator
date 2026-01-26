package com.beepboop.app

import com.beepboop.parser.NewMinizincParserBaseListener
import com.beepboop.parser.NewMinizincParser.{Enum_itemContext, Var_decl_itemContext}
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import com.beepboop.app.dataprovider.*

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Try

class MinizincModelListener(tokens: CommonTokenStream, extendedDataType: Boolean = true) extends NewMinizincParserBaseListener {

  private val dataItemsBuffer = new ListBuffer[DataItem]()

  def getDataItems: List[DataItem] = dataItemsBuffer.toList

  private def textWithSpaces(ctx: ParserRuleContext): String = {
    if (ctx == null) return ""
    val start = ctx.getStart.getTokenIndex
    val stop = ctx.getStop.getTokenIndex
    tokens.getText(new Interval(start, stop))
  }

  private def parseSimpleValue(text: String): Any = {
    val t = text.trim
    if (t == "true") return true
    if (t == "false") return false

    Try(t.toInt).toOption match {
      case Some(i) => return i
      case None =>
    }

    Try(t.toDouble).toOption match {
      case Some(d) => return d
      case None =>
    }

    None
  }

  override def enterEnum_item(ctx: Enum_itemContext): Unit = {
    val name = ctx.ident().getText

    var expr = ""
    var fullType = "set of int"

    val rawText = textWithSpaces(ctx)
    if (rawText.contains("=")) {
      val parts = rawText.split("=", 2)
      if (parts.length > 1) {
        val rhs = parts(1).trim.stripSuffix(";")

        if (rhs.startsWith("_(") && rhs.endsWith(")")) {
          expr = rhs.substring(2, rhs.length - 1).trim
        } else {
          expr = rhs
        }
      }
    }

    val detailedType = DataType(
      dataType = "int",
      isArray = false,
      isIdentifier = false,
      isSet = true
    )

    val dataItem = DataItem(
      name = name,
      dataType = fullType,
      isVar = false,
      value = None,
      detailedDataType = detailedType,
      expr = expr
    )

    dataItemsBuffer += dataItem
  }

  override def enterVar_decl_item(ctx: Var_decl_itemContext): Unit = {
    assert(ctx != null)
    var name =
      if (ctx.ident() == null) ctx.ti_expr_and_id().ident().getText
      else ctx.ident().getText

    var fullType = ""
    var detailedFullType = DataType(null, false, false)
    var expr = ""
    var isVar = false
    var initialValue: Any = None

    if (ctx.ti_expr_and_id().ti_expr().array_ti_expr() != null) {
      val arrayTi = ctx.ti_expr_and_id().ti_expr().array_ti_expr()
      val baseTi = arrayTi.base_ti_expr()

      val isSetType = baseTi.set_ti() != null && baseTi.set_ti().getText.toLowerCase.contains("set")

      if (baseTi.var_par().getText != "var") {
        fullType = textWithSpaces(arrayTi)

        val typeName = if (baseTi.base_ti_expr_tail().base_type() == null) {
          textWithSpaces(baseTi.base_ti_expr_tail().ident())
        } else {
          textWithSpaces(baseTi.base_ti_expr_tail().base_type())
        }

        val isIdent = baseTi.base_ti_expr_tail().base_type() == null

        detailedFullType = DataType(typeName, true, isIdent, isSet = isSetType)

        if (ctx.expr() != null) {
          expr = ctx.expr().getText
        }
      } else {
        fullType = textWithSpaces(arrayTi)
        val tail = baseTi.base_ti_expr_tail()

        if (tail.base_type() == null) {
          if (tail.ident() != null) {
            detailedFullType = DataType(textWithSpaces(tail.ident()), true, true, isSet = isSetType)
          } else {
            detailedFullType = DataType(textWithSpaces(arrayTi), true, true, isSet = isSetType)
          }
        } else {
          detailedFullType = DataType(textWithSpaces(tail.base_type()), true, false, isSet = isSetType)
        }
        isVar = true
      }
    } else if (ctx.ti_expr_and_id().ti_expr().base_ti_expr() != null) {
      val baseTi = ctx.ti_expr_and_id().ti_expr().base_ti_expr()
      val isSetType = baseTi.set_ti() != null && baseTi.set_ti().getText.toLowerCase.contains("set")

      if (baseTi.var_par().getText != "var") {
        fullType = textWithSpaces(baseTi)
        val typeName = if (baseTi.base_ti_expr_tail().base_type() != null) {
          textWithSpaces(baseTi.base_ti_expr_tail().base_type())
        } else {
          baseTi.base_ti_expr_tail().getText
        }
        detailedFullType = DataType(
          dataType = typeName,
          isArray = false,
          isIdentifier = false,
          isSet = isSetType
        )
      } else {
        fullType = textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr())
        if (ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().base_type() == null) {
          if (ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().ident() != null) {
            detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().ident()), false, true)
          } else {
            detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr()), false, true)
          }
        } else {
          detailedFullType = DataType(textWithSpaces(ctx.ti_expr_and_id().ti_expr().base_ti_expr().base_ti_expr_tail().base_type()), false, false)
        }
        isVar = true
      }

      if (ctx.expr() != null) {
        expr = ctx.expr().getText
      }
    }

    if (ctx.expr() != null) {
      initialValue = parseSimpleValue(ctx.expr().getText)
    }

    val dataItem = if (extendedDataType) {
      DataItem(name = name, dataType = fullType, isVar = isVar, value = initialValue, detailedDataType = detailedFullType, expr = expr)
    } else {
      DataItem(name = name, dataType = fullType, isVar = isVar, value = initialValue, detailedDataType = null, expr = expr)
    }
    dataItemsBuffer += dataItem
  }
}