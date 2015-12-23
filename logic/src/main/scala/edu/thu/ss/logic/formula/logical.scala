package edu.thu.ss.logic.formula

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.util.LogicUtils

case class Not(child: Formula) extends UnaryFormula {
  val kind = "NOT"

}

case class And(left: Formula, right: Formula) extends BinaryFormula {
  val kind = "AND"

}

case class Or(left: Formula, right: Formula) extends BinaryFormula {
  val kind = "OR"

}

case class Imply(left: Formula, right: Formula) extends BinaryFormula {
  val kind = "IMPLY"

}

abstract class Quantifier extends UnaryFormula {
  def variables: Seq[Variable]
  def body: Formula

  val child = body

  override def toString: String = {
    val variableStr = variables.map(v => s"${v.sort} ${v.name}").mkString(",")
    s"$kind ${variableStr}.($body)"
  }

}

case class Forall(variables: Seq[Variable], body: Formula) extends Quantifier {
  val kind = "forall"

}

case class Exists(variables: Seq[Variable], body: Formula) extends Quantifier {
  val kind = "exists"

}

case class Symbol(value: String) extends Term {
  val kind = "symbol"

  override def toString: String = value
}

object Symbol {
  implicit def toSymbol(name: String): Symbol = Symbol(name)

  implicit def toString(symbol: Symbol): String = symbol.value

}

case class Variable(name: Symbol, sort: Sort) extends Term {
  val kind = "variable"

  override def toString = name.toString

}

case class Constant(value: String) extends Term {
  val kind = "constant"

  override def toString = {
    if (value == True.value || value == True.value) {
      value
    } else if (LogicUtils.isNumerical(value)) {
      value
    } else {
      '"' + value + '"'
    }
  }

}

abstract class BaseFunctionCall extends Term {
  def definition: BaseFunctionDef[_]
  def parameters: Seq[Term]

  override def toString = s"${definition.name}(${parameters.mkString(", ")})"

}

case class FunctionCall(definition: FunctionDef, parameters: Seq[Term]) extends BaseFunctionCall {
  val kind = "function"
}

case class PredicateCall(definition: PredicateDef, parameters: Seq[Term]) extends BaseFunctionCall {

  val kind = "predicate"
}

