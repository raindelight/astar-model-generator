package com.beepboop.app

import com.beepboop.app.logger.LogTrait
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, Lexer, Parser, TokenStream}
import org.antlr.v4.runtime.tree.ParseTree
import com.typesafe.scalalogging.*



object ParserUtil extends LogTrait {

  def parseCode[L <: Lexer, P <: Parser](
                                          code: String,
                                          lexerFactory: (CharStream) => L,
                                          parserFactory: (TokenStream) => P,
                                          startRuleInvoker: (P) => ParseTree
                                        ): (P, ParseTree) = { 

    val input: CharStream = CharStreams.fromString(code)

    val lexer: L = lexerFactory(input)

    val tokens: CommonTokenStream = new CommonTokenStream(lexer)

    val parser: P = parserFactory(tokens)

    val tree: ParseTree = startRuleInvoker(parser)

    debug("--- Parse tree (LISP style) ---")
    debug(tree.toStringTree(parser))

    (parser, tree)
  }
}