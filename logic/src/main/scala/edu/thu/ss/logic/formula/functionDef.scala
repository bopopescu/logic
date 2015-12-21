package edu.thu.ss.logic.formula

case class Parameter(name: Symbol, sort: Sort) extends Term {
  override def kind = "parameter"

  override def toString = name.toString
}



abstract class BaseFunctionDef extends LogicDefinition {
  def name: Symbol
  def parameters: Seq[Parameter]
  def range: Sort

  def domain: Seq[Sort] = parameters.map(_.sort)

  override def toString = s"$range $name(${parameters.mkString(", ")})"

}

case class FunctionDef(name: Symbol, parameters: Seq[Parameter], range: Sort, clazz: Class[_]) extends BaseFunctionDef {
  override def kind = "function"

}

case class PredicateDef(name: Symbol, parameters: Seq[Parameter], clazz: Class[_]) extends BaseFunctionDef {
  override def kind = "predicate"
  override def range: Sort = BoolSort

}




