package edu.thu.ss.logic.paser

import scala.annotation.migration
import scala.util.parsing.combinator.JavaTokenParsers

import edu.thu.ss.logic.formula._

import edu.thu.ss.logic.formula.Symbol._

class FormulaParser extends JavaTokenParsers {

  protected val NOT = """NOT|not""".r
  protected val AND = """AND|and""".r
  protected val OR = """OR|or""".r
  protected val IMPLY = """IMPLY|imply""".r
  protected val FORALL = """FORALL|forall""".r
  protected val EXISTS = """EXISTS|exists""".r
  protected val TRUE = """TRUE|true""".r
  protected val FALSE = """FALSE|false""".r

  def parse(str: String): Option[Formula] = {
    val result = parseAll(parseFormula, str)
    if (result.successful) {
      return Some(result.get)
    } else {
      return None
    }
  }

  protected def parseFormula: Parser[Formula] = parseQuantifier

  protected def matchQuantifier = opt(NOT) ~ (FORALL | EXISTS) ~ rep1sep(ident ~ ident ^^ {
    case variable ~ sort => UnresolvedVariable(variable, UnresolvedSort(sort))
  }, ",") <~ "."

  protected def parseQuantifier: Parser[Formula] = rep(matchQuantifier) ~ parseU ^^ {
    case list ~ u => {
      list.foldRight(u) {
        case ((not ~ qual ~ variables), child) => {
          val formula = qual.toLowerCase match {
            case "forall" => UnresolvedForall(variables, child)
            case "exists" => UnresolvedExists(variables, child)
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
    rep1sep(parseImply, "AU") ^^ (_.reduceRight { AU(_, _) }) |
      rep1sep(parseImply, "EU") ^^ (_.reduceRight { EU(_, _) }) |
      rep1sep(parseImply, "pAU") ^^ (_.reduceRight { pAU(_, _) }) |
      rep1sep(parseImply, "pEU") ^^ (_.reduceRight { pEU(_, _) })

  // right associative
  protected def parseImply: Parser[Formula] = rep1sep(parseOr, IMPLY) ^^ (_.reduceRight { Imply(_, _) })

  protected def parseOr: Parser[Formula] = parseAnd * (OR ^^^ { Or(_, _) })

  protected def parseAnd: Parser[Formula] = parseUnary * (AND ^^^ { And(_, _) })

  protected def parseUnary: Parser[Formula] = NOT ~> parseUnary ^^ (Not(_)) |
    "(" ~> parseFormula <~ ")" |
    "AG" ~> parseUnary ^^ (AG(_)) |
    "AF" ~> parseUnary ^^ (AF(_)) |
    "AX" ~> parseUnary ^^ (AX(_)) |
    "EG" ~> parseUnary ^^ (EG(_)) |
    "EF" ~> parseUnary ^^ (EF(_)) |
    "EX" ~> parseUnary ^^ (EX(_)) |
    "pAG" ~> parseUnary ^^ (pAG(_)) |
    "pAF" ~> parseUnary ^^ (pAF(_)) |
    "pAX" ~> parseUnary ^^ (pAX(_)) |
    "pEG" ~> parseUnary ^^ (pEG(_)) |
    "pEF" ~> parseUnary ^^ (pEF(_)) |
    "pEX" ~> parseUnary ^^ (pEX(_)) |
    parseTerm

  protected def parseTerm: Parser[Term] = parseFunction |
    ident ^^ (UnresolvedVariable(_, UnknownSort)) |
    TRUE ^^ (_ => True) |
    FALSE ^^ (_ => False) |
    (stringLiteral |
      decimalNumber) ^^ (Constant(_))

  protected def parseFunction: Parser[Term] = ident ~ opt("(" ~> repsep(parseTerm, ",") <~ ")") ^^ {
    case name ~ params =>
      if (params.isDefined) {
        UnresolvedFunctionCall(name, params.get)
      } else {
        UnresolvedFunctionCall(name, Nil)

      }
  }

}