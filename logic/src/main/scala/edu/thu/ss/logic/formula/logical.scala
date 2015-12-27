package edu.thu.ss.logic.formula

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.util.LogicUtils
import edu.thu.ss.logic.definition.IBaseFunction

case class Not(child: Formula) extends UnaryFormula {
  val nodeName = "NOT"

}

case class And(left: Formula, right: Formula) extends BinaryFormula {
  val nodeName = "AND"

}

case class Or(left: Formula, right: Formula) extends BinaryFormula {
  val nodeName = "OR"

}

case class Imply(left: Formula, right: Formula) extends BinaryFormula {
  val nodeName = "IMPLY"

}

abstract class Quantifier extends UnaryFormula {
  def variable: Variable
  def body: Formula

  val child = body

  var quantifiedPredicate: PredicateCall = null

  override def toString: String = {
    s"$nodeName ${variable}. $body"
  }

}

case class Forall(variable: Variable, body: Formula) extends Quantifier {
  val nodeName = "forall"

}

case class Exists(variable: Variable, body: Formula) extends Quantifier {
  val nodeName = "exists"

}

case class Symbol(value: String) extends Term {
  val nodeName = "symbol"

  override def toString: String = value
}

object Symbol {
  implicit def toSymbol(name: String): Symbol = Symbol(name)

  implicit def toString(symbol: Symbol): String = symbol.value

}

case class Variable(name: Symbol, sort: Sort) extends Term {
  val nodeName = "variable"

  override def toString = name.toString

}

case class Constant(value: Any) extends Term {
  val nodeName = "constant"

  override def toString = {
    if (LogicUtils.isNumericalValue(value)) {
      value.toString
    } else {
      '"' + value.toString + '"'
    }
  }

}

abstract class BaseFunctionCall extends Term {
  def definition: BaseFunctionDef[_ <: IBaseFunction]
  def parameters: Seq[Term]

  override def toString = s"${definition.name}(${parameters.mkString(", ")})"

}

case class FunctionCall(definition: FunctionDef, parameters: Seq[Term]) extends BaseFunctionCall {
  val nodeName = "function"
}

case class PredicateCall(definition: PredicateDef, parameters: Seq[Term]) extends BaseFunctionCall {

  val nodeName = "predicate"
}

