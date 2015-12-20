package edu.thu.ss.logic.formula

case class Parameter(name: Symbol, sort: Sort) extends Term {
  override def getName = "Parameter"

  override def toString = name.toString
}

abstract class LogicDefinition {
  def name: Symbol
  def clazz: Class[_]

}

abstract class BaseFunctionDef extends LogicDefinition {
  def name: Symbol
  def parameters: Seq[Parameter]
  def range: Sort

  def domain: Seq[Sort] = parameters.map(_.sort)
}

case class FunctionDef(name: Symbol, parameters: Seq[Parameter], range: Sort, clazz: Class[_]) extends BaseFunctionDef {

}

case class PredicateDef(name: Symbol, parameters: Seq[Parameter], clazz: Class[_]) extends BaseFunctionDef {
  override def range: Sort = BoolSort

}




