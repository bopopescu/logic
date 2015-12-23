package edu.thu.ss.logic.formula

abstract class Formula extends TreeNode[Formula] {

}

trait NamedFormula extends ASTNode {
  def name: Symbol
  var formula: Formula

}

object NamedFormula {
  implicit def toFormula(named: NamedFormula) = named.formula

}

abstract class BinaryFormula extends Formula {
  def left: Formula
  def right: Formula

  val children: Seq[Formula] = left :: right :: Nil

  override def toString = s"($left $kind $right)"
}

abstract class UnaryFormula extends Formula {
  def child: Formula

  lazy val children: Seq[Formula] = child :: Nil

  override def toString = s"$kind $child"
}

abstract class Term extends Formula {
  val children: Seq[Formula] = Nil

}

