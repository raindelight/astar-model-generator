package com.beepboop.app

import com.beepboop.app.logger.LogTrait
import com.beepboop.app.{G4ToInternalRepresentationListener, MinizincModelListener, ParserUtil}
import com.beepboop.parser.*
import com.typesafe.scalalogging.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, TokenStream}

import java.nio.file.{Files, Paths}
import scala.io.Source

object GrammarConverter extends LogTrait {

  def parseConstraintGrammar(): ParsedGrammar = {
    val grammarString = scala.io.Source.fromFile("src/main/antlr4/com/beepboop/parser/ModelConstraintGrammar.g4").mkString
    val (metaParser, g4ParseTree) = ParserUtil.parseCode(
      code = grammarString,
      lexerFactory = (cs: CharStream) => new ANTLRv4Lexer(cs),
      parserFactory = (ts: TokenStream) => new ANTLRv4Parser(ts),
      startRuleInvoker = (p: ANTLRv4Parser) => p.grammarSpec()
    )

    info("\nParsing g4 grammar to internal representation")

    val listener = new G4ToInternalRepresentationListener(specialVarSymbolName = "var_symbol_placeholder")
    val walker = new ParseTreeWalker()
    walker.walk(listener, g4ParseTree)

    val internalGrammar: ParsedGrammar = listener.getParsedGrammar
    internalGrammar
  }

}


