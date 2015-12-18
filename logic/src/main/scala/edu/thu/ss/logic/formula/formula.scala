package edu.thu.ss.logic.formula

abstract class Formula {
  def children: Seq[Formula]

  def getName: String
}

abstract class BinaryFormula extends Formula {
  def left: Formula
  def right: Formula

  override def children: Seq[Formula] = left :: right :: Nil

  override def toString() = s"($left $getName $right)"

}

abstract class UnaryFormula extends Formula {
  def child: Formula

  override def children: Seq[Formula] = child :: Nil

  override def toString() = s"$getName $child"
}

case class Not(child: Formula) extends UnaryFormula {

  override def getName: String = "NOT"

}

case class And(left: Formula, right: Formula) extends BinaryFormula {
  override def getName: String = "AND"

}

case class Or(left: Formula, right: Formula) extends BinaryFormula {
  override def getName: String = "OR"

}

case class Imply(left: Formula, right: Formula) extends BinaryFormula {
  override def getName: String = "IMPLY"

}

abstract class Quantifier extends Formula {
  def variables: Seq[Variable]
  def body: Formula

  override def children: Seq[Formula] = body :: Nil

  override def toString: String = {
    val variableStr = variables.map(v => s"${v.name} ${v.sort}").mkString(",")
    s"$getName ${variableStr}.($body)"
  }

}

case class Forall(variables: Seq[Variable], body: Formula) extends Quantifier {
  override def getName: String = "FORALL"

}

case class Exists(variables: Seq[Variable], body: Formula) extends Quantifier {
  override def getName: String = "EXISTS"

}

case class Symbol(name: String) {

  override def toString: String = name
}

object Symbol {
  implicit def toSymbol(name: String): Symbol = Symbol(name)

}

abstract class Term extends Formula {
  override def children: Seq[Formula] = Nil

}

case class Variable(name: Symbol, sort: Sort) extends Term {
  override def getName = "VARIABLE"

  override def toString = name.toString

}

case class Constant(value: Any) extends Term {
  override def getName = "CONSTANT"

  override def toString = value.toString

}

case object True extends Term {
  override def getName = "TRUE"

}

case object False extends Term {
  override def getName = "FALSE"

}
