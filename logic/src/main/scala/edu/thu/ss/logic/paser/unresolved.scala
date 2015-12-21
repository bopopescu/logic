package edu.thu.ss.logic.paser

import edu.thu.ss.logic.formula.Formula
import edu.thu.ss.logic.formula.Symbol
import edu.thu.ss.logic.formula.Term
import edu.thu.ss.logic.formula.Variable
import edu.thu.ss.logic.formula.ASTNode
import edu.thu.ss.logic.policy.FormulaWrapper

trait UnresolvedElement extends ASTNode {
  def name: Symbol

}

case class UnresolvedFunctionCall(name: Symbol, parameters: Seq[Term]) extends Term with UnresolvedElement {

  override def kind = name.toString

  override def toString = s"$kind(${parameters.mkString(", ")})"
}

class UnresolvedVariable(override val name: Symbol, val usort: Symbol) extends Variable(name, null) with UnresolvedElement {

  override def toString = name.toString
}

abstract class UnresolvedDefinition extends UnresolvedElement {
  def clazz: String

}

case class UnresolvedSort(name: Symbol, clazz: String) extends UnresolvedDefinition {
  override def toString = name.toString

  override def kind = "sort"

}

abstract class UnresolvedBaseFunctionDef extends UnresolvedDefinition {
  def parameters: Seq[UnresolvedParameter]
  def range: Symbol
  override def toString = s"$range $name(${parameters.mkString(", ")})"

}

case class UnresolvedFunctionDef(name: Symbol, parameters: Seq[UnresolvedParameter], range: Symbol, clazz: String) extends UnresolvedBaseFunctionDef {
  override def toString = s"$range $name(${parameters.mkString(", ")})"

  override def kind = "function"
}

case class UnresolvedPredicateDef(name: Symbol, parameters: Seq[UnresolvedParameter], clazz: String) extends UnresolvedBaseFunctionDef {
  val range: Symbol = ""

  override def toString = s"bool $name(${parameters.mkString(", ")})"

  override def kind = "predicate"

}

case class UnresolvedFormulaDef(name: Symbol, formula: Formula) extends UnresolvedDefinition{
  override val clazz = ""
  override val kind = "formula"
}

case class UnresolvedParameter(name: Symbol, sort: Symbol) extends UnresolvedElement {
  val kind = "Parameter"

  override def toString = s"$sort $name"
}


