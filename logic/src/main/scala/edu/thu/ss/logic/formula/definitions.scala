package edu.thu.ss.logic.formula

import scala.collection.mutable
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.formula.Symbol.toString
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.policy.FormulaWrapper

abstract class LogicDefinition extends ASTNode {
  def name: Symbol
  def clazz: Class[_]
}

case class FormulaDef(name: Symbol, var formula: Formula) extends LogicDefinition with FormulaWrapper {
  override def kind = "formula"
  override val clazz = null
}

class LogicDefinitions {
  private val sorts: mutable.Map[Symbol, Sort] = new mutable.HashMap

  private val functions: mutable.Map[Symbol, FunctionDef] = new mutable.HashMap

  private val predicates: mutable.Map[Symbol, PredicateDef] = new mutable.HashMap

  private val formulas: mutable.Map[Symbol, FormulaDef] = new mutable.HashMap

  def addSort(sort: Sort) {
    if (sorts.contains(sort.name)) {
      throw new AnalysisException(sort.name)
    }

    sorts.put(sort.name, sort)
  }

  def addFunction(function: FunctionDef) {
    if (functions.contains(function.name)) {
      throw new AnalysisException(function.name)
    }

    functions.put(function.name, function)
  }

  def addPredicate(predicate: PredicateDef) {
    if (predicates.contains(predicate.name)) {
      throw new AnalysisException(predicate.name)
    }

    predicates.put(predicate.name, predicate)
  }

  def addFormula(formula: FormulaDef) {
    if (formulas.contains(formula.name)) {
      throw new AnalysisException(formula.name)
    }
    formulas.put(formula.name, formula)
  }

  def lookupSort(name: Symbol) = sorts.get(name)
  def lookupFunction(name: Symbol) = functions.get(name)
  def lookupPredicate(name: Symbol) = predicates.get(name)
  def lookupFormula(name: Symbol) = formulas.get(name)

  def getFormulas = formulas

  override def toString = s"""${sorts.values.mkString("\n")}
${functions.values.mkString("\n")}
${predicates.values.mkString("\n")}"""

}