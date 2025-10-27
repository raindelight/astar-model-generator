package com.beepboop.app

import com.beepboop.app.{GrammarSymbol, ProductionAlternative, ProductionRule}

sealed trait GrammarSymbol {
  def name: String
}

case class NonTerminal(override val name: String) extends GrammarSymbol {
  override def toString: String = s"<$name>"
}

case class Terminal(override val name: String, value: String) extends GrammarSymbol {
  override def toString: String = if (name == value || name.startsWith("'")) s"'$value'" else name
}

case class VarPlaceholder(override val name: String) extends GrammarSymbol {
  override def toString: String = s"VAR_PLACEHOLDER[${name}]"
}

case class Optional(symbol: GrammarSymbol, isGreedy: Boolean = true) extends GrammarSymbol {
  override def name: String = s"${symbol.name}?"
  override def toString: String = s"(${symbol.toString})?"
}

case class ZeroOrMore(symbol: GrammarSymbol, isGreedy: Boolean = true) extends GrammarSymbol {
  override def name: String = s"${symbol.name}*"
  override def toString: String = s"(${symbol.toString})*"
}

case class OneOrMore(symbol: GrammarSymbol, isGreedy: Boolean = true) extends GrammarSymbol {
  override def name: String = s"${symbol.name}+"
  override def toString: String = s"(${symbol.toString})+"
}


case class ProductionAlternative(symbols: List[GrammarSymbol]) {
  override def toString: String = symbols.mkString(" ")
}


case class ProductionRule(
                           ruleName: String,
                           alternatives: List[ProductionAlternative]
                         ) {
  override def toString: String = s"$ruleName ::= ${alternatives.map(_.toString).mkString(" | ")}"
}

case class ParsedGrammar(
                          grammarName: String,
                          startSymbol: Option[String],
                          rules: Map[String, ProductionRule],
                          tokens: Map[String, String]
                        ) {
  override def toString: String = {
    s"""
       |Grammar: $grammarName
       |Start Symbol: ${startSymbol.getOrElse("Not explicitly defined, assuming first rule")}
       |Tokens:
       |${tokens.map{case (k,v) => s"  $k : '$v'"}.mkString("\n")}
       |Rules:
       |${rules.values.map("  " + _.toString).mkString("\n")}
    """.stripMargin
  }
}