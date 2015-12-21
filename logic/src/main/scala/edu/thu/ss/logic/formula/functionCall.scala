package edu.thu.ss.logic.formula

abstract class BaseFunctionCall extends Term {
  def definition: BaseFunctionDef
  def parameters: Seq[Term]

  override def kind = "function"

  override def toString = s"${definition.name}(${parameters.mkString(", ")})"
}

case class FunctionCall(definition: FunctionDef, parameters: Seq[Term]) extends BaseFunctionCall {

}

case class PredicateCall(definition: PredicateDef, parameters: Seq[Term]) extends BaseFunctionCall {

}

