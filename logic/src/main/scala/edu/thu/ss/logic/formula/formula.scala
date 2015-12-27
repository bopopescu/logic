package edu.thu.ss.logic.formula

import edu.thu.ss.logic.tree.TreeNode

abstract class Formula extends TreeNode[Formula] {

}

trait NamedFormula {
  def name: Symbol
  var formula: Formula

}

object NamedFormula {
  implicit def toFormula(named: NamedFormula) = named.formula

}

abstract class BinaryFormula extends Formula {
  def left: Formula
  def right: Formula

  lazy val children: Seq[Formula] = left :: right :: Nil
  override def toString = s"($left $nodeName $right)"
}

abstract class UnaryFormula extends Formula {
  def child: Formula

  lazy val children: Seq[Formula] = child :: Nil

  override def toString = s"$nodeName $child"
}

abstract class Term extends Formula {
  lazy val children: Seq[Formula] = Nil

}

