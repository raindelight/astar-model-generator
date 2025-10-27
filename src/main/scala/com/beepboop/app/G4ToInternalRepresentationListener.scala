package com.beepboop.app

import com.beepboop.app.astar.*
import com.beepboop.app.logger.LogTrait
import com.beepboop.parser.ANTLRv4Parser.*
import com.beepboop.parser.ANTLRv4ParserBaseListener
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class G4ToInternalRepresentationListener(val specialVarSymbolName: String = "var_symbol_placeholder") extends ANTLRv4ParserBaseListener, LogTrait {

  private var grammarName: String = "UnknownGrammar"
  private val rules = mutable.Map[String, ProductionRule]()
  private val tokenLiterals = mutable.Map[String, String]()
  private val declaredTokenNames = mutable.Set[String]()

  private var currentRuleName: Option[String] = None
  private var currentAlternatives: mutable.ListBuffer[ProductionAlternative] = mutable.ListBuffer()
  private var currentSymbolsInAlternative: mutable.ListBuffer[GrammarSymbol] = mutable.ListBuffer()
  private var firstParserRuleName: Option[String] = None

  def getParsedGrammar: ParsedGrammar = {
    val finalTokensMap = declaredTokenNames.map { tokenName =>
      tokenName -> tokenLiterals.getOrElse(tokenName, s"[$tokenName Pattern]")
    }.toMap
    ParsedGrammar(grammarName, firstParserRuleName, rules.toMap, finalTokensMap)
  }

  private def stripQuotes(text: String): String = {
    if (text == null) return ""
    if (text.length >= 2) {
      if (text.startsWith("'") && text.endsWith("'")) {
        text.substring(1, text.length - 1)
      } else if (text.startsWith("\"") && text.endsWith("\"")) {
        text.substring(1, text.length - 1)
      } else text
    } else text
  }

  override def enterGrammarSpec(ctx: GrammarSpecContext): Unit = {
    if (ctx.grammarDecl() != null && ctx.grammarDecl().identifier() != null) {
      val idCtx = ctx.grammarDecl().identifier()
      if (idCtx.RULE_REF() != null) grammarName = idCtx.RULE_REF().getText
      else if (idCtx.TOKEN_REF() != null) grammarName = idCtx.TOKEN_REF().getText
      else grammarName = "GrammarNameError"
      warn(s"DEBUG [enterGrammarSpec]: Grammar name set to '$grammarName'")
    } else {
      warn("WARNING [enterGrammarSpec]: Could not determine grammar name from grammarDecl.")
    }
  }

  override def enterTokensSpec(ctx: TokensSpecContext): Unit = {
    if (ctx.idList() != null) {
      ctx.idList().identifier().asScala.foreach { idCtx =>
        val tokenName = if (idCtx.TOKEN_REF() != null) idCtx.TOKEN_REF().getText
        else if (idCtx.RULE_REF() != null) idCtx.RULE_REF().getText
        else "UNKNOWN_TOKEN_IN_TOKENS_BLOCK"
        declaredTokenNames += tokenName
        warn(s"DEBUG [enterTokensSpec]: Declared token: '$tokenName'")
      }
    }
  }

  override def enterLexerRuleSpec(ctx: LexerRuleSpecContext): Unit = {
    val tokenName = ctx.TOKEN_REF().getText
    warn(s"DEBUG [enterLexerRuleSpec]: Processing potential token definition for: '$tokenName'")

    if (tokenName == specialVarSymbolName) {
      warn(s"DEBUG [enterLexerRuleSpec]: Skipping '$tokenName' as it matches specialVarSymbolName and should be a parser rule.")
      return
    }
    declaredTokenNames += tokenName
    try {
      if (ctx.lexerRuleBlock() != null &&
        ctx.lexerRuleBlock().lexerAltList() != null &&
        !ctx.lexerRuleBlock().lexerAltList().lexerAlt().isEmpty &&
        ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements() != null &&
        !ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements().lexerElement().isEmpty &&
        ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements().lexerElement(0).lexerAtom() != null &&
        ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements().lexerElement(0).lexerAtom().terminalDef() != null &&
        ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements().lexerElement(0).lexerAtom().terminalDef().STRING_LITERAL() != null) {
        val literal = stripQuotes(ctx.lexerRuleBlock().lexerAltList().lexerAlt(0).lexerElements().lexerElement(0).lexerAtom().terminalDef().STRING_LITERAL().getText)
        tokenLiterals += (tokenName -> literal)
        warn(s"DEBUG [enterLexerRuleSpec]: Defined token: '$tokenName' with literal: '$literal'")
      } else {
        warn(s"DEBUG [enterLexerRuleSpec]: Declared token: '$tokenName' (complex pattern, no simple literal extracted)")
      }
    } catch {
      case e: Exception => warn(s"DEBUG [enterLexerRuleSpec]: Error parsing literal for token '$tokenName', declared as pattern.")
    }
  }

  override def enterParserRuleSpec(ctx: ParserRuleSpecContext): Unit = {
    val ruleName = ctx.RULE_REF().getText
    currentRuleName = Some(ruleName)
    currentAlternatives = mutable.ListBuffer()
    if (firstParserRuleName.isEmpty) {
      firstParserRuleName = Some(ruleName)
    }
    warn(s"DEBUG [enterParserRuleSpec]: Entering rule '$ruleName'")
  }

  override def exitParserRuleSpec(ctx: ParserRuleSpecContext): Unit = {
    currentRuleName.foreach { name =>
      if (currentAlternatives.nonEmpty) {
        rules += (name -> ProductionRule(name, currentAlternatives.toList))
        warn(s"DEBUG [exitParserRuleSpec]: Added rule '$name' with ${currentAlternatives.length} alternatives.")
      } else {
        warn(s"WARNING [exitParserRuleSpec]: Rule '$name' has no alternatives, not adding to map.")
      }
    }
    currentRuleName = None
  }

  override def enterAlternative(ctx: AlternativeContext): Unit = {
    currentSymbolsInAlternative = mutable.ListBuffer()
    // warn(s"DEBUG [enterAlternative]: For rule ${currentRuleName.getOrElse("UNKNOWN")}")
  }

  override def exitAlternative(ctx: AlternativeContext): Unit = {
    currentAlternatives += ProductionAlternative(currentSymbolsInAlternative.toList)
  }

  override def enterElement(ctx: ElementContext): Unit = {
    var baseSymbolOpt: Option[GrammarSymbol] = None
    val currentElementText = ctx.getText

    if (ctx.atom() != null) {
      val atom = ctx.atom()

      if (atom.ruleref() != null) {
        val ruleName = atom.ruleref().RULE_REF().getText
        warn(s"DEBUG [enterElement for '$currentElementText']: Found RULEREF: '$ruleName'. specialVarSymbolName is '$specialVarSymbolName'")
        if (ruleName == specialVarSymbolName) {
          warn(s"DEBUG [enterElement]: Matched specialVarSymbolName. Creating VarPlaceholder for '$ruleName'.")
          baseSymbolOpt = Some(VarPlaceholder(ruleName))
        } else {
          baseSymbolOpt = Some(NonTerminal(ruleName))
        }
      }
      else if (atom.terminalDef() != null) {
        val terminalCtx = atom.terminalDef()
        if (terminalCtx.TOKEN_REF() != null) {
          val tokenName = terminalCtx.TOKEN_REF().getText
          warn(s"DEBUG [enterElement for '$currentElementText']: Found TOKEN_REF: '$tokenName'")
          val tokenActualValue = tokenLiterals.getOrElse(tokenName, s"[$tokenName PatternRef]")
          baseSymbolOpt = Some(Terminal(tokenName, tokenActualValue))
        } else if (terminalCtx.STRING_LITERAL() != null) {
          val literalValue = stripQuotes(terminalCtx.STRING_LITERAL().getText)
          warn(s"DEBUG [enterElement for '$currentElementText']: Found STRING_LITERAL: '$literalValue'")
          baseSymbolOpt = Some(Terminal(literalValue, literalValue))
        }
      } else if (atom.wildcard() != null) {
        val dotText = atom.wildcard().getText
        baseSymbolOpt = Some(Terminal(dotText, dotText))
      }

  } else if (ctx.ebnf() != null) {
      val ebnfCtx = ctx.ebnf()
      if (ebnfCtx.block() != null && ebnfCtx.block().altList() != null && !ebnfCtx.block().altList().alternative().isEmpty) {
        val blockText = ebnfCtx.block().getText
        warn(s"WARNING [enterElement->ebnf]: EBNF block '$blockText' encountered. Simplified handling.")
        baseSymbolOpt = Some(NonTerminal(s"[[EBNF_BLOCK: ${ebnfCtx.block().getText()}]]"))

      } else {
        baseSymbolOpt = Some(NonTerminal(s"[[EBNF_EMPTY_OR_SIMPLE: ${ebnfCtx.getText()}]]"))
      }
    } else if (ctx.actionBlock() != null) {
    warn(s"DEBUG [enterElement]: Skipping actionBlock: ${ctx.actionBlock().getText}")
    return
  }

    baseSymbolOpt.foreach { sym =>
      var finalSymbol: GrammarSymbol = sym
      if (ctx.ebnfSuffix() != null) {
        val suffixText = ctx.ebnfSuffix().getText
        suffixText.charAt(0) match {
          case '?' => finalSymbol = Optional(sym)
          case '*' => finalSymbol = ZeroOrMore(sym)
          case '+' => finalSymbol = OneOrMore(sym)
          case _   =>
        }
      }
      currentSymbolsInAlternative += finalSymbol
    }
  }
}
