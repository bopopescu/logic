package edu.thu.ss.logic.evaluation

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
/**
 * a context for each model
 */
class EvaluationContext() {

  private val functionImpls = new mutable.HashMap[Symbol, IBaseFunction]

  private val valuation = new mutable.HashMap[Symbol, Any]

  def getFunctionImpl(function: FunctionDef): IFunction = {
    getBaseFunctionImpl(function).asInstanceOf[IFunction]
  }

  def getPredicateImpl(predicate: PredicateDef): IPredicate = {
    getBaseFunctionImpl(predicate).asInstanceOf[IPredicate]
  }

  def getBaseFunctionImpl(base: BaseFunctionDef[_ <: IBaseFunction]): IBaseFunction = {
    functionImpls.getOrElseUpdate(base.name, base.impl).asInstanceOf[IBaseFunction]
  }

  def getValue(variable: Variable) = valuation.get(variable.name).get

  def setValue(variable: Variable, value: Any) {
    valuation.put(variable.name, value)
  }

  def removeValue(variable: Variable) {
    valuation.remove(variable.name)
  }

}

class FormulaEvaluator(val context: EvaluationContext) {

  def evaluate(formula: Formula): Boolean = {
    formula match {
      case Not(child) => !evaluate(child)
      case And(left, right) => evaluate(left) && evaluate(right)
      case Or(left, right) => evaluate(left) || evaluate(right)
      case Imply(left, right) =>
        if (evaluate(left))
          evaluate(right)
        else
          true
      case pred: PredicateCall => evaluateFunction(pred).asInstanceOf[Boolean]
      case quantifier: Quantifier =>
        evaluateQuantifier(quantifier)
      case variable: Variable =>
        //must be a boolean variable
        context.getValue(variable).asInstanceOf[Boolean]
      case True => true
      case False => false
    }

  }

  private def evaluateFunction(function: BaseFunctionCall): Any = {
    val functionDef = function.definition
    val params = function.parameters.map(evaluateParam(_))
    val impl = context.getBaseFunctionImpl(functionDef)
    functionDef.evaluate(impl, params)
  }

  private def evaluateParam(param: Term): Any = {
    param match {
      case subfunc: BaseFunctionCall => evaluateFunction(subfunc)
      case const: Constant => const.value
      case variable: Variable =>
        // must be initialized
        context.getValue(variable)
    }
  }

  private def evaluateQuantifier(quantifier: Quantifier): Boolean = {
    val values = getQuantifiedValues(quantifier)
    val transformed = quantifier.child.transform {
      //ignore the evaluation of the quantified predicate  
      case pred: PredicateCall if (pred.eq(quantifier.quantifiedPredicate)) => True
    }

    val result =
      quantifier match {
        case forall: Forall => values.forall(value => {
          context.setValue(forall.variable, value)
          evaluate(transformed)
        })
        case exists: Exists => values.exists(value => {
          context.setValue(exists.variable, value)
          evaluate(transformed)
        })
      }
    context.removeValue(quantifier.variable)
    result
  }

  private def getQuantifiedValues(quantifier: Quantifier): Traversable[Any] = {
    val variable = quantifier.variable
    if (quantifier.quantifiedPredicate != null) {
      val pred = quantifier.quantifiedPredicate
      val impl = context.getPredicateImpl(pred.definition)

      val index = pred.parameters.indexOf(variable)
      val otherParams = pred.parameters.withFilter(_ != variable).map(evaluateParam(_))
      val values = impl.quantifiedValues(index, otherParams)
      //TODO: should we check values?
      values.foreach(value => {
        if (!variable.sort.validValue(value)) {
          throw new IllegalValueException(s"$value returned by ${pred.definition.clazz} is not a valid value of ${variable.sort.kind} ${variable.sort.name}.")
        }
      })
      values
    } else {
      variable.sort.values
    }
  }

}