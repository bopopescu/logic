package edu.thu.ss.logic.formula

abstract class BaseFunctionCall extends Term {
  def decl: BaseFunctionDecl
  def parameters: Seq[Term]

  override def getName = decl.name.toString

  override def toString = s"$getName(${parameters.mkString(", ")})"
}

case class FunctionCall(decl: FunctionDecl, parameters: Seq[Term]) extends BaseFunctionCall {

}

case class PredicateCall(decl: PredicateDecl, parameters: Seq[Term]) extends BaseFunctionCall {

}

