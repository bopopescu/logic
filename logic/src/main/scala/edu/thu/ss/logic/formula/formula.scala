package edu.thu.ss.logic.formula

trait ASTNode {
  def kind: String

}

abstract class Formula extends ASTNode {
  def children: Seq[Formula]

}

abstract class BinaryFormula extends Formula {
  def left: Formula
  def right: Formula

  override def children: Seq[Formula] = left :: right :: Nil

  override def toString() = s"($left $kind $right)"

}

abstract class UnaryFormula extends Formula {
  def child: Formula

  override def children: Seq[Formula] = child :: Nil

  override def toString() = s"$kind $child"
}

case class Not(child: Formula) extends UnaryFormula {

  override def kind: String = "NOT"

}

case class And(left: Formula, right: Formula) extends BinaryFormula {
  override def kind: String = "AND"

}

case class Or(left: Formula, right: Formula) extends BinaryFormula {
  override def kind: String = "OR"

}

case class Imply(left: Formula, right: Formula) extends BinaryFormula {
  override def kind: String = "IMPLY"

}

abstract class Quantifier extends Formula {
  def variables: Seq[Variable]
  def body: Formula

  override def children: Seq[Formula] = body :: Nil

  override def toString: String = {
    val variableStr = variables.map(v => s"${v.sort} ${v.name}").mkString(",")
    s"$kind ${variableStr}.($body)"
  }

}

case class Forall(variables: Seq[Variable], body: Formula) extends Quantifier {
  override def kind: String = "FORALL"

}

case class Exists(variables: Seq[Variable], body: Formula) extends Quantifier {
  override def kind: String = "EXISTS"

}

case class Symbol(value: String) extends Term {
  override def kind = "symbol"

  override def toString: String = value
}

object Symbol {
  implicit def toSymbol(name: String): Symbol = Symbol(name)

  implicit def toString(symbol: Symbol): String = symbol.value

}

abstract class Term extends Formula {
  override def children: Seq[Formula] = Nil

}

case class Variable(name: Symbol, sort: Sort) extends Term {
  override def kind = "variable"

  override def toString = name.toString

}

case class Constant(value: Any) extends Term {
  override def kind = "constant"

  override def toString = value.toString

}

case object True extends Term {
  override def kind = "true"

}

case object False extends Term {
  override def kind = "false"

}
