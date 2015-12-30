package edu.thu.ss.logic.paser

import java.io
import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.io.StringReader

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader._
import scala.util.parsing.input.StreamReader

import edu.thu.ss.logic.formula
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.formula.Symbol._
import edu.thu.ss.logic.policy.Rule

trait LogicKeywords {

  val NOT = "not"
  val NOT_S = "!"

  val AND = "and"
  val AND_S = "&&"

  val OR = "or"
  val OR_S = "||"

  val IMPLY = "imply"
  val IMPLY_S = "->"

  val AG = "ag"
  val AX = "ax"
  val AF = "af"
  val AU = "au"
  val EG = "eg"
  val EX = "ex"
  val EF = "ef"
  val EU = "eu"
  val pAG = "pag"
  val pAX = "pax"
  val pAF = "paf"
  val pAU = "pau"
  val pEG = "peg"
  val pEX = "pex"
  val pEF = "pef"
  val pEU = "peu"

  val FORALL = "forall"
  val EXISTS = "exists"
  val TRUE = "true"
  val FALSE = "false"

  val DEFINE = "define"
  val SORT = "sort"
  val FUNCTION = "function"
  val PREDICATE = "predicate"
  val FORMULA = "formula"
  val CLASS = "class"

  val DEFINITION = "#definition"
  val POLICY = "#policy"

}

class LogicLexical extends StdLexical with LogicKeywords {

  /* This is a work around to support the lazy setting */
  def initialize(keywords: Seq[String]): Unit = {
    reserved.clear()
    reserved ++= keywords
  }

  delimiters += ("(", ")", ",", ":", ".", "=", "+", "-", NOT_S, AND_S, OR_S, IMPLY_S)

  override def identChar = letter | elem('_') | elem('#')

  protected override def processIdent(name: String) = {
    val token = name.toLowerCase
    if (reserved contains token) Keyword(token) else Identifier(name)
  }

  override lazy val token: Parser[Token] =
    (identChar ~ (identChar | digit).* ^^
      { case first ~ rest => processIdent((first :: rest).mkString) }
      | digit.* ~ identChar ~ (identChar | digit).* ^^
      { case first ~ middle ~ rest => processIdent((first ++ (middle :: rest)).mkString) }
      | rep1(digit) ~ ('.' ~> digit.*).? ^^ {
        case i ~ None => NumericLit(i.mkString)
        case i ~ Some(d) => NumericLit(i.mkString + "." + d.mkString)
      }
      | '\'' ~> chrExcept('\'', '\n', EofCh).* <~ '\'' ^^
      { case chars => StringLit(chars mkString "") }
      | '"' ~> chrExcept('"', '\n', EofCh).* <~ '"' ^^
      { case chars => StringLit(chars mkString "") }
      | '`' ~> chrExcept('`', '\n', EofCh).* <~ '`' ^^
      { case chars => Identifier(chars mkString "") }
      | EofCh ^^^ EOF
      | '\'' ~> failure("unclosed string literal")
      | '"' ~> failure("unclosed string literal")
      | delim
      | failure("illegal character"))
}

/**
 * provides parse primitives based on scala parser combinator
 */
object LogicParser extends StandardTokenParsers with LogicKeywords {

  def parse(in: String): (Seq[UnresolvedDefinition], Seq[Rule]) = {
    parse(new StringReader(in))
  }

  def parseFile(path: String): (Seq[UnresolvedDefinition], Seq[Rule]) = {
    val file = new File(path)
    if (!file.exists()) {
      throw ParseException(s"No such file: $path.")
    }
    parse(new BufferedReader(new FileReader(file)))
  }

  def parse(in: io.Reader): (Seq[UnresolvedDefinition], Seq[Rule]) = {
    try {
      phrase(parseInput)(new lexical.Scanner(StreamReader(in))) match {
        case Success(r, _) => r
        case fail => throw ParseException(fail.toString)
      }
    } finally {
      in.close()
    }
  }

  override val lexical = new LogicLexical

  protected val reserved: Seq[String] = {
    classOf[LogicKeywords].getDeclaredMethods().filter(_.getReturnType() == classOf[String])
      .map(_.invoke(this).asInstanceOf[String].toLowerCase())
  }

  lexical.initialize(this.reserved)

  def parseInput: Parser[(Seq[UnresolvedDefinition], Seq[Rule])] =
    (DEFINITION ~> rep(parseDef)) ~ (POLICY ~> rep(parseRule)) ^^ {
      case list1 ~ list2 => (list1, list2)
    }

  def parseDef: Parser[UnresolvedDefinition] =
    (parseSort | parseFunctionDef | parsePredicateDef | parseFormulaDef)

  protected def parseSort: Parser[UnresolvedSort] =
    (DEFINE ~> SORT ~> ident) ~ parseClass ^^ {
      case name ~ clazz => UnresolvedSort(name, clazz)
    }

  protected def parseFunctionDef: Parser[UnresolvedFunctionDef] =
    (DEFINE ~> FUNCTION ~> ident ~ ident) ~ parseParameterList ~ parseClass ^^ {
      case range ~ name ~ params ~ clazz => UnresolvedFunctionDef(name, params, range, clazz)
    }

  protected def parsePredicateDef: Parser[UnresolvedPredicateDef] =
    (DEFINE ~> PREDICATE ~> ident) ~ parseParameterList ~ parseClass ^^ {
      case name ~ params ~ clazz => UnresolvedPredicateDef(name, params, clazz)
    }

  protected def parseFormulaDef: Parser[UnresolvedFormulaDef] =
    (DEFINE ~> FORMULA ~> ident) ~ ("=" ~> parseFormula) ^^ {
      case name ~ formula => UnresolvedFormulaDef(name, formula)
    }

  protected def parseParameterList: Parser[Seq[UnresolvedParameter]] =
    "(" ~> repsep(ident ~ ident, ",") <~ ")" ^^ (
      _.map {
        case sort ~ name => UnresolvedParameter(name, sort)
      })

  protected def parseClass: Parser[String] =
    CLASS ~> "=" ~> rep1sep(ident, ".") ^^ (_.mkString("."))

  //rules
  def parseRule: Parser[Rule] = ident ~ (":" ~> parseFormula) ^^ {
    case name ~ formula => Rule(name, formula)
  }

  def parseFormula: Parser[Formula] = parseQuantifier

  protected def parseQuantifier: Parser[Formula] =
    rep(opt(NOT | NOT_S) ~ (FORALL | EXISTS) ~ parseVariableList <~ ".") ~ parseU ^^ {
      case list ~ u => {
        val transformed = list.flatMap {
          case not ~ qual ~ variables =>
            var first = true
            variables.map { v =>
              val _not = if (first) {
                first = false
                not
              } else {
                None
              }
              (_not, qual, v)
            }
        }
        transformed.foldRight(u) {
          case ((not, qual, variable), child) =>
            val formula = qual.toLowerCase match {
              case FORALL => Forall(variable, child)
              case EXISTS => Exists(variable, child)
            }
            not match {
              case Some(_) => Not(formula)
              case None => formula
            }
        }
      }
    }

  protected def parseVariableList: Parser[Seq[UnresolvedVariable]] =
    rep1sep(ident ~ ident, ",") ^^ (_.map {
      case sort ~ name => new UnresolvedVariable(name, sort)
    })

  //right associative
  protected def parseU: Parser[Formula] =
    rep1sep(parseImply, AU) ^^
      (_.reduceRight { formula.AU(_, _) }) |
      //TODO only keep AU
      rep1sep(parseImply, EU) ^^
      (_.reduceRight { formula.AU(_, _) }) |
      rep1sep(parseImply, pAU) ^^
      (_.reduceRight { formula.pAU(_, _) }) |
      rep1sep(parseImply, pEU) ^^
      (_.reduceRight { formula.pEU(_, _) })

  // right associative
  protected def parseImply: Parser[Formula] =
    rep1sep(parseOr, IMPLY | IMPLY_S) ^^ (_.reduceRight { Imply(_, _) })

  protected def parseOr: Parser[Formula] =
    parseAnd * ((OR | OR_S) ^^^ { Or(_, _) })

  protected def parseAnd: Parser[Formula] =
    parseUnary * ((AND | AND_S) ^^^ { And(_, _) })

  protected def parseUnary: Parser[Formula] =
    "(" ~> parseFormula <~ ")" |
      (NOT | NOT_S) ~> parseUnary ^^ (Not(_)) |
      AG ~> parseUnary ^^ (formula.AG(_)) |
      AF ~> parseUnary ^^ (formula.AF(_)) |
      AX ~> parseUnary ^^ (formula.AX(_)) |
      //TODO: only keep AG/AF
      EG ~> parseUnary ^^ (formula.AG(_)) |
      EF ~> parseUnary ^^ (formula.AF(_)) |
      EX ~> parseUnary ^^ (formula.EX(_)) |
      pAG ~> parseUnary ^^ (formula.pAG(_)) |
      pAF ~> parseUnary ^^ (formula.pAF(_)) |
      pAX ~> parseUnary ^^ (formula.pAX(_)) |
      pEG ~> parseUnary ^^ (formula.pEG(_)) |
      pEF ~> parseUnary ^^ (formula.pEF(_)) |
      pEX ~> parseUnary ^^ (formula.pEX(_)) |
      parseBoolTerm

  protected def parseBoolTerm: Parser[Term] = parseFunction |
    ident ^^ (Symbol(_)) |
    TRUE ^^ (_ => True) |
    FALSE ^^ (_ => False)

  protected def parseFunction: Parser[Term] =
    ident ~ ("(" ~> repsep(parseTerm, ",") <~ ")") ^^ {
      case name ~ params =>
        UnresolvedFunctionCall(name, params)
    }

  protected def parseTerm: Parser[Term] =
    parseBoolTerm |
      (stringLit | number) ^^ (UnresolvedConstant(_))

  protected lazy val number: Parser[String] =
    ("-").? ~ numericLit ^^ {
      case sign ~ number =>
        sign match {
          case Some(s) => s + number
          case None => number
        }
    }

}