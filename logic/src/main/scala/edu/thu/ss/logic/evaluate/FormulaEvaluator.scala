package edu.thu.ss.logic.evaluate

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.model.State
/**
 * a context for each model
 */
class EvaluationContext(val model: QueryModel) {

  private val functionImpls = new mutable.HashMap[Symbol, IBaseFunction]

  private val valuation = new mutable.HashMap[Symbol, Any]

  def init {
    functionImpls.clear
    valuation.clear
  }

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

class FormulaEvaluator(val model: QueryModel) {

  private lazy val context = new EvaluationContext(model)

  def evaluate(formula: Formula): Boolean = {
    context.init
    model.initialStates.forall(evaluateFormula(formula, _))

  }

  private def evaluateFormula(formula: Formula, state: State): Boolean = {
    formula match {
      case Not(child) => !(evaluateFormula(child, state))
      case And(left, right) => evaluateFormula(left, state) && evaluateFormula(right, state)
      case Or(left, right) => evaluateFormula(left, state) || evaluateFormula(right, state)
      case Imply(left, right) =>
        if (evaluateFormula(left, state))
          evaluateFormula(right, state)
        else
          true
      case pred: PredicateCall => evaluateFunction(pred, state).asInstanceOf[Boolean]
      case quantifier: Quantifier =>
        evaluateQuantifier(quantifier, state)
      case variable: Variable =>
        //must be a boolean variable
        context.getValue(variable).asInstanceOf[Boolean]
      case True => true
      case False => false
      //temporal part
      case ag: AG =>
        true
      case af: AF =>
        true
      case au: AU =>
        true
      case ax: AX =>
        true
      case eg: EG =>
        true
      case ef: EF =>
        true
      case eu: EU =>
        true
      case ex: EX =>
        true

      case pag: pAG =>
        true
      case paf: pAF =>
        true
      case pau: pAU =>
        true
      case pax: pAX =>
        true
      case peg: pEG =>
        true
      case pef: pEF =>
        true
      case peu: pEU =>
        true
      case pex: pEX =>
        true
    }

  }

  private def evaluateFunction(function: BaseFunctionCall, state: State): Any = {
    val functionDef = function.definition
    val impl = context.getBaseFunctionImpl(functionDef)
    val params = function.parameters.map(evaluateParam(_, state))
    functionDef.evaluate(impl, state, params)
  }

  private def evaluateParam(param: Term, state: State): Any = {
    param match {
      case subfunc: BaseFunctionCall => evaluateFunction(subfunc, state)
      case const: Constant => const.value
      case variable: Variable =>
        // must be initialized
        context.getValue(variable)
    }
  }

  private def evaluateQuantifier(quantifier: Quantifier, state: State): Boolean = {
    val values = getQuantifiedValues(quantifier, state)
    val transformed = quantifier.child.transform {
      //ignore the evaluation of the quantified predicate  
      case pred: PredicateCall if (pred.eq(quantifier.quantifiedPredicate)) => True
    }

    val result =
      quantifier match {
        case forall: Forall => values.forall(value => {
          context.setValue(forall.variable, value)
          evaluateFormula(transformed, state)
        })
        case exists: Exists => values.exists(value => {
          context.setValue(exists.variable, value)
          evaluateFormula(transformed, state)
        })
      }
    context.removeValue(quantifier.variable)
    result
  }

  private def getQuantifiedValues(quantifier: Quantifier, state: State): Traversable[Any] = {
    val variable = quantifier.variable
    if (quantifier.quantifiedPredicate != null) {
      val pred = quantifier.quantifiedPredicate
      val impl = context.getPredicateImpl(pred.definition)

      val index = pred.parameters.indexOf(variable)
      val otherParams = pred.parameters.withFilter(_ != variable).map(evaluateParam(_, state))
      val values = impl.quantifiedValues(index, otherParams)
      //TODO: should we check values?
      values.foreach(value => {
        if (!variable.sort.validValue(value)) {
          throw new IllegalValueException(s"$value returned by ${pred.definition.clazz} is not a valid value of ${variable.sort.nodeName} ${variable.sort.name}.")
        }
      })
      values
    } else {
      variable.sort.values
    }
  }

}