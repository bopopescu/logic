package edu.thu.ss.logic.policy

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.formula.LogicDefinitions
import scala.collection.mutable

trait FormulaWrapper extends ASTNode {
  var formula: Formula
  def name: Symbol

}

object FormulaWrapper {
  implicit def toFormula(wrapper: FormulaWrapper): Formula = wrapper.formula

}

case class Rule(name: Symbol, var formula: Formula) extends ASTNode with FormulaWrapper {

  override def kind = "rule"

  override def toString = s"$name: $formula"
}

class Policy(val definitions: LogicDefinitions, val rules: Seq[Rule]) extends ASTNode {

  override def kind = "policy"

  override def toString = s"""#defition
${definitions.toString}

#policy
${rules.mkString("\n")}"""

}