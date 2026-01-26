package com.beepboop.app

import com.beepboop.parser.NewMinizincParserBaseVisitor
import com.beepboop.parser.NewMinizincParser
import com.beepboop.app.dataprovider.DataItem
import scala.jdk.CollectionConverters._

class MinizincDznVisitor(dataItems: List[DataItem]) extends NewMinizincParserBaseVisitor[Any] {

  private val itemMap = dataItems.map(i => i.name -> i).toMap

  override def visitAssign_item(ctx: NewMinizincParser.Assign_itemContext): Any = {
    val id = if (ctx.ident() != null) ctx.ident().getText else ctx.getChild(0).getText
    val valueExpr = ctx.expr()

    itemMap.get(id) match {
      case Some(item) =>
        val parsedValue = visit(valueExpr)
        item.value = parsedValue
      case None =>
    }
    null
  }

  override def visitExpr(ctx: NewMinizincParser.ExprContext): Any = {
    val left = visit(ctx.expr_atom())

    if (ctx.expr_binop_tail() != null && ctx.expr_binop_tail().bin_op() != null) {
      val binOp = ctx.expr_binop_tail().bin_op()
      if (binOp.builtin_bin_op() != null && binOp.builtin_bin_op().DOTDOT() != null) {
        val right = visit(ctx.expr_binop_tail().expr())
        return (left, right) match {
          case (l: Int, r: Int) => (l to r).toSet
          case _ => left
        }
      }
    }
    left
  }

  override def visitExpr_atom(ctx: NewMinizincParser.Expr_atomContext): Any = {
    val headValue = if (ctx.expr_atom_head() != null) visit(ctx.expr_atom_head()) else null

    if (headValue == "array2d" && ctx.expr_atom_tail() != null) {
      val tail = ctx.expr_atom_tail()

      if (tail.getStart.getType == NewMinizincParser.LPAREN) {

        val args = tail.getRuleContexts(classOf[NewMinizincParser.ExprContext])
          .asScala
          .map(visit)
          .toList

        if (args.length == 3) {
          val colsArg = args(1)
          val listArg = args(2) match {
            case l: List[_] => l
            case other => List(other)
          }

          val colCount = colsArg match {
            case i: Int => i
            case s: Set[_] => s.size
            case l: List[_] => l.size
            case _ => 0
          }

          if (colCount > 0) {
            return listArg.grouped(colCount).toList
          }
        }
      }
    }
    headValue
  }

  override def visitExpr_atom_head(ctx: NewMinizincParser.Expr_atom_headContext): Any = {
    if (ctx.int_literal() != null) return visit(ctx.int_literal())
    if (ctx.float_literal() != null) return visit(ctx.float_literal())
    if (ctx.bool_literal() != null) return visit(ctx.bool_literal())
    if (ctx.string_literal() != null) return visit(ctx.string_literal())
    if (ctx.array_literal() != null) return visit(ctx.array_literal())
    if (ctx.array_literal_2d() != null) return visit(ctx.array_literal_2d())
    if (ctx.set_literal() != null) return visit(ctx.set_literal())

    if (ctx.LPAREN() != null && ctx.expr() != null) return visit(ctx.expr())

    ctx.getText
  }

  override def visitInt_literal(ctx: NewMinizincParser.Int_literalContext): Any =
    ctx.getText.toInt

  override def visitFloat_literal(ctx: NewMinizincParser.Float_literalContext): Any =
    ctx.getText.toDouble

  override def visitBool_literal(ctx: NewMinizincParser.Bool_literalContext): Any =
    ctx.getText.toBoolean

  override def visitString_literal(ctx: NewMinizincParser.String_literalContext): Any = {
    val txt = ctx.getText
    if (txt.length >= 2) txt.substring(1, txt.length - 1) else txt
  }

  override def visitArray_literal(ctx: NewMinizincParser.Array_literalContext): Any = {
    if (ctx.expr() == null) return List.empty
    ctx.expr().asScala.map(visit).toList
  }

  override def visitArray_literal_2d(ctx: NewMinizincParser.Array_literal_2dContext): Any = {
    if (ctx.array_row() == null) return List.empty
    ctx.array_row().asScala.map { rowCtx =>
      rowCtx.expr().asScala.map(visit).toList
    }.toList
  }

  override def visitSet_literal(ctx: NewMinizincParser.Set_literalContext): Any = {
    if (ctx.expr() == null) return Set.empty
    ctx.expr().asScala.map(visit).toSet
  }
}