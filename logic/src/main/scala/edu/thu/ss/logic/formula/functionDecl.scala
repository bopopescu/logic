package edu.thu.ss.logic.formula

case class Parameter(name: Symbol, sort: Sort) extends Term {
  override def getName = "Parameter"

  override def toString = name.toString
}

abstract class BaseFunctionDecl {
  def name: Symbol
  def parameters: Seq[Parameter]
  def range: Sort

  def domain: Seq[Sort] = parameters.map(_.sort)
}

case class FunctionDecl(name: Symbol, parameters: Seq[Parameter], range: Sort) extends BaseFunctionDecl {

}

case class PredicateDecl(name: Symbol, parameters: Seq[Parameter]) extends BaseFunctionDecl {
  override def range: Sort = BoolSort

}




