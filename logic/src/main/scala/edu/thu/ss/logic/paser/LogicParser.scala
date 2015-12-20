package edu.thu.ss.logic.paser

import scala.annotation.migration
import scala.util.parsing.combinator.JavaTokenParsers
import edu.thu.ss.logic.formula
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.formula.Symbol._
import edu.thu.ss.logic.policy.Rule
import edu.thu.ss.logic.policy.Rule
import edu.thu.ss.logic.policy.Policy
import org.scalatest.enablers.Definition
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._
import scala.util.parsing.input.Reader
import java.io.StringReader
import java.io
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.StreamReader
import java.io.File
import java.io.BufferedReader
import java.io.FileReader

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

  delimiters += ("(", ")", ",", ":", ".", "=", NOT_S, AND_S, OR_S, IMPLY_S)

  override def identChar = letter | elem('_') | elem('#')

  protected override def processIdent(name: String) = {
    val token = name.toLowerCase
    if (reserved contains token) Keyword(token) else Identifier(name)
  }
}

/**
 * provides parse primitives based on scala parser combinator
 */
object LogicParser extends StandardTokenParsers with LogicKeywords {

  def parse(in: String): (List[UnresolvedDefinition], List[Rule]) = {
    parse(new StringReader(in))
  }

  def parseFile(path: String): (List[UnresolvedDefinition], List[Rule]) = {
    val file = new File(path)
    if (!file.exists()) {
      throw ParseException(s"No such file: $path.")
    }
    parse(new BufferedReader(new FileReader(file)))
  }

  def parse(in: io.Reader): (List[UnresolvedDefinition], List[Rule]) = {
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

  def parseInput: Parser[(List[UnresolvedDefinition], List[Rule])] = (DEFINITION ~> rep(parseDef)) ~ (POLICY ~> rep(parseRule)) ^^ {
    case list1 ~ list2 => (list1, list2)
  }

  def parseDef: Parser[UnresolvedDefinition] = (parseSort | parseFunctionDef | parsePredicateDef)

  def parseSort: Parser[UnresolvedSort] = (DEFINE ~> SORT ~> ident) ~ parseClass ^^ {
    case name ~ clazz => UnresolvedSort(name, clazz)
  }

  def parseFunctionDef: Parser[UnresolvedFunctionDef] = (DEFINE ~> FUNCTION ~> ident ~ ident) ~ parseParameterList ~ parseClass ^^ {
    case range ~ name ~ params ~ clazz => UnresolvedFunctionDef(name, params, range, clazz)

  }

  def parsePredicateDef: Parser[UnresolvedPredicateDef] = (DEFINE ~> PREDICATE ~> ident) ~ parseParameterList ~ parseClass ^^ {
    case name ~ params ~ clazz => UnresolvedPredicateDef(name, params, clazz)
  }

  def parseParameterList: Parser[Seq[UnresolvedParameter]] = "(" ~> repsep(ident ~ ident, ",") <~ ")" ^^ {
    case list => list.map(v => UnresolvedParameter(v._2, v._1))
  }

  def parseRule: Parser[Rule] = ident ~ (":" ~> parseFormula) ^^ {
    case name ~ formula => Rule(name, formula)
  }

  def parseFormula: Parser[Formula] = parseQuantifier

  protected def parseClass: Parser[String] = CLASS ~> "=" ~> rep1sep(ident, ".") ^^ (_.mkString("."))

  protected def matchQuantifier = opt(NOT | NOT_S) ~ (FORALL | EXISTS) ~ rep1sep(ident ~ ident ^^ {
    case sort ~ variable => new UnresolvedVariable(variable, sort)
  }, ",") <~ "."

  protected def parseQuantifier: Parser[Formula] = rep(matchQuantifier) ~ parseU ^^ {
    case list ~ u => {
      list.foldRight(u) {
        case ((not ~ qual ~ variables), child) => {
          val formula = qual.toLowerCase match {
            case FORALL => Forall(variables, child)
            case EXISTS => Exists(variables, child)
          }
          if (not.isDefined) {
            Not(formula)
          } else {
            formula
          }
        }
      }
    }
  }

  //right associative
  protected def parseU: Parser[Formula] =
    rep1sep(parseImply, AU) ^^ (_.reduceRight { formula.AU(_, _) }) |
      rep1sep(parseImply, EU) ^^ (_.reduceRight { formula.EU(_, _) }) |
      rep1sep(parseImply, pAU) ^^ (_.reduceRight { formula.pAU(_, _) }) |
      rep1sep(parseImply, pEU) ^^ (_.reduceRight { formula.pEU(_, _) })

  // right associative
  protected def parseImply: Parser[Formula] = rep1sep(parseOr, IMPLY | IMPLY_S) ^^ (_.reduceRight { Imply(_, _) })

  protected def parseOr: Parser[Formula] = parseAnd * ((OR | OR_S) ^^^ { Or(_, _) })

  protected def parseAnd: Parser[Formula] = parseUnary * ((AND | AND_S) ^^^ { And(_, _) })

  protected def parseUnary: Parser[Formula] =
    "(" ~> parseFormula <~ ")" |
      (NOT | NOT_S) ~> parseUnary ^^ (Not(_)) |
      AG ~> parseUnary ^^ (formula.AG(_)) |
      AF ~> parseUnary ^^ (formula.AF(_)) |
      AX ~> parseUnary ^^ (formula.AX(_)) |
      EG ~> parseUnary ^^ (formula.EG(_)) |
      EF ~> parseUnary ^^ (formula.EF(_)) |
      EX ~> parseUnary ^^ (formula.EX(_)) |
      pAG ~> parseUnary ^^ (formula.pAG(_)) |
      pAF ~> parseUnary ^^ (formula.pAF(_)) |
      pAX ~> parseUnary ^^ (formula.pAX(_)) |
      pEG ~> parseUnary ^^ (formula.pEG(_)) |
      pEF ~> parseUnary ^^ (formula.pEF(_)) |
      pEX ~> parseUnary ^^ (formula.pEX(_)) |
      parseTerm

  protected def parseTerm: Parser[Term] = parseFunction |
    ident ^^ (Symbol(_)) |
    TRUE ^^ (_ => True) |
    FALSE ^^ (_ => False) |
    (stringLit | numericLit) ^^ (Constant(_))

  protected def parseFunction: Parser[Term] = ident ~ opt("(" ~> repsep(parseTerm, ",") <~ ")") ^^ {
    case name ~ params =>
      if (params.isDefined) {
        UnresolvedFunctionCall(name, params.get)
      } else {
        UnresolvedFunctionCall(name, Nil)
      }
  }

}