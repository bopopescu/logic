package edu.thu.ss.logic.paser

import edu.thu.ss.logic.formula._

trait UnresolvedElement {
  def name: Symbol

}

case class UnresolvedFunctionCall(name: Symbol, parameters: Seq[Term]) extends Term with UnresolvedElement {
  val kind = name.toString

  override def toString = s"$kind(${parameters.mkString(", ")})"
}

class UnresolvedVariable(override val name: Symbol, val usort: Symbol) extends Variable(name, null) with UnresolvedElement {

  override def toString = name.toString
}

abstract class UnresolvedDefinition extends ASTNode with UnresolvedElement {
  def clazz: String

}

case class UnresolvedSort(name: Symbol, clazz: String) extends UnresolvedDefinition {
  val kind = "sort"

  override def toString = name

}

case class UnresolvedParameter(name: Symbol, sort: Symbol) extends UnresolvedElement {
  val kind = "parameter"

  override def toString = s"$sort $name"
}

abstract class UnresolvedBaseFunctionDef extends UnresolvedDefinition {
  def parameters: Seq[UnresolvedParameter]
  def range: Symbol

  override def toString = s"$range $name(${parameters.mkString(", ")})"

}

case class UnresolvedFunctionDef(name: Symbol, parameters: Seq[UnresolvedParameter], range: Symbol, clazz: String) extends UnresolvedBaseFunctionDef {
  override def toString = s"$range $name(${parameters.mkString(", ")})"

  val kind = "function"
}

case class UnresolvedPredicateDef(name: Symbol, parameters: Seq[UnresolvedParameter], clazz: String) extends UnresolvedBaseFunctionDef {
  val range: Symbol = "bool"

  override def toString = s"bool $name(${parameters.mkString(", ")})"

  val kind = "predicate"

}

case class UnresolvedFormulaDef(name: Symbol, formula: Formula) extends UnresolvedDefinition {
  val clazz = ""

  val kind = "formula"
}
