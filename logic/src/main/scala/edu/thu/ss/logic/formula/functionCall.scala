package edu.thu.ss.logic.formula

abstract class BaseFunctionCall extends Term {
  def decl: BaseFunctionDef
  def parameters: Seq[Term]

  override def getName = decl.name.toString

  override def toString = s"$getName(${parameters.mkString(", ")})"
}

case class FunctionCall(decl: FunctionDef, parameters: Seq[Term]) extends BaseFunctionCall {

}

case class PredicateCall(decl: PredicateDef, parameters: Seq[Term]) extends BaseFunctionCall {

}

