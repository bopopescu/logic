package edu.thu.ss.logic.evaluation

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
/**
 * a context for each model
 */
class EvaluationContext() {

  private val functionImpls = new mutable.HashMap[Symbol, IBaseFunction]

  def getFunctionImpl(function: FunctionDef): IFunction = {
    getBaseFunctionImpl(function).asInstanceOf[IFunction]
  }

  def getPredicateImpl(predicate: PredicateDef): IPredicate = {
    getBaseFunctionImpl(predicate).asInstanceOf[IPredicate]
  }

  def getBaseFunctionImpl(base: BaseFunctionDef[_ <: IBaseFunction]): IBaseFunction = {
    functionImpls.getOrElseUpdate(base.name, base.impl).asInstanceOf[IBaseFunction]
  }

}

class FormulaEvaluator(val context: EvaluationContext) {

  def evaluate(formula: Formula): Boolean = {
    formula match {
      case Not(child) => !evaluate(child)

      case And(left, right) => evaluate(left) && evaluate(right)

      case Or(left, right) => evaluate(left) || evaluate(right)

      case Imply(left, right) =>
        if (evaluate(left)) {
          evaluate(right)
        } else {
          true
        }

      case True => true

      case False => false

      case pred: PredicateCall => evaluateFunction(pred).asInstanceOf[Boolean]

    }

  }

  private def evaluateFunction(function: BaseFunctionCall): Any = {
    val functionDef = function.definition
    val sorts = functionDef.domain.iterator
    val params = function.parameters.map(
      param => {
        val sort = sorts.next
        param match {
          case subfunc: BaseFunctionCall => evaluateFunction(subfunc)
          case const: Constant =>
            sort.toValue(const.value)
        }
      })

    val impl = context.getBaseFunctionImpl(functionDef)
    functionDef.evaluate(impl, params)
  }

}