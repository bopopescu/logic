package edu.thu.ss.logic.paser

import edu.thu.ss.logic.formula.Formula
import edu.thu.ss.logic.formula.Symbol
import edu.thu.ss.logic.formula.Term
import edu.thu.ss.logic.formula.Variable

trait UnresolvedElement {
  def name: Symbol

}

case class UnresolvedFunctionCall(name: Symbol, parameters: Seq[Term]) extends Term with UnresolvedElement {

  override def getName = name.toString

  override def toString = s"$getName(${parameters.mkString(", ")})"
}

class UnresolvedVariable(override val name: Symbol, val usort: Symbol) extends Variable(name, null) with UnresolvedElement {

  override def toString = name.toString
}

abstract class UnresolvedDefinition extends UnresolvedElement {
  def clazz: String

}

case class UnresolvedSort(name: Symbol, clazz: String) extends UnresolvedDefinition {
  override def toString = name.toString

}

case class UnresolvedFunctionDef(name: Symbol, parameters: Seq[UnresolvedParameter], range: Symbol, clazz: String) extends UnresolvedDefinition {
  override def toString = s"$range $name(${parameters.mkString(", ")})"
}

case class UnresolvedPredicateDef(name: Symbol, parameters: Seq[UnresolvedParameter], clazz: String) extends UnresolvedDefinition {
  override def toString = s"bool $name(${parameters.mkString(", ")})"
}

case class UnresolvedParameter(name: Symbol, sort: Symbol) extends UnresolvedElement {
  override def toString = s"$sort $name"
}
