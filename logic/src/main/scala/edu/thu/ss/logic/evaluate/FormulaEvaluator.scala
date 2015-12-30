package edu.thu.ss.logic.evaluate

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.model.State
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.util.LogicUtils
/**
 * a context for each model
 */
class EvaluationContext(val model: QueryModel) {

  private val functionImpls = new mutable.HashMap[Symbol, IBaseFunction]

  private val valuation = new mutable.HashMap[Symbol, Any]

  def clear {
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

  def isDefined(variable: Variable) = valuation.contains(variable.name)

  def removeValue(variable: Variable) {
    valuation.remove(variable.name)
  }

}

class FormulaEvaluator(val model: QueryModel) extends Logging {

  private lazy val context = new EvaluationContext(model)

  def evaluate(formula: Formula): Boolean = {
    val value = model.initialStates.forall(evaluateFormula(formula, _))

    context.clear
    model.clearCache

    value
  }

  private def evaluateFormula(formula: Formula, state: State): Boolean = {
    val cacheEnabled = !LogicUtils.hasValuedVariables(formula, context)

    if (cacheEnabled) {
      val cacheResult = state.getFormula(formula)
      if (cacheResult.isDefined) {
        logTrace(s"cache hit for formula $formula")
        return cacheResult.get
      }
    }

    val result =
      formula match {
        case Not(child) =>
          !(evaluateFormula(child, state))

        case And(left, right) =>
          evaluateFormula(left, state) && evaluateFormula(right, state)

        case Or(left, right) =>
          evaluateFormula(left, state) || evaluateFormula(right, state)

        case Imply(left, right) =>
          !evaluateFormula(left, state) || evaluateFormula(right, state)

        case pred: PredicateCall =>
          evaluateFunction(pred, state).asInstanceOf[Boolean]

        case quantifier: Quantifier =>
          evaluateQuantifier(quantifier, state)

        case variable: Variable =>
          //must be a boolean variable
          context.getValue(variable).asInstanceOf[Boolean]

        case True => true

        case False => false

        //temporal part
        case _: AG | _: EG =>
          val g = formula.asInstanceOf[UnaryFormula]
          state.forallParent(evaluateFormula(g.child, _))

        case _: AF | _: EF =>
          //child holds in one of the current plus all parent states
          val f = formula.asInstanceOf[UnaryFormula]
          state.existsParent { evaluateFormula(f.child, _) }

        case _: AU | _: EU =>
          //either right holds in the current state, or left holds in the current state and u holds in the parent state
          val u = formula.asInstanceOf[BinaryFormula with TemporalFormula]
          evaluateFormula(u.right, state) ||
            (state.parent != null && evaluateFormula(u.left, state) && evaluateFormula(u, state.parent))

        case ax: AX =>
          //either the parent state does not exist, or child holds in the parent state        
          state.parent == null || evaluateFormula(ax.child, state.parent)
        case ex: EX =>
          //the parent state must exist, and child holds in the parent state
          state.parent != null || evaluateFormula(ex.child, state.parent)

        case pag: pAG =>
          //child holds in the current plus all children states
          state.forall { evaluateFormula(pag.child, _) }
        case peg: pEG =>
          //child holds in the current state or one of the children states (if any) 
          evaluateFormula(peg.child, state) &&
            (state.children.isEmpty || state.children.exists { evaluateFormula(peg, _) })

        case paf: pAF =>
          //either child holds in the current state, or paf holds in all children states (non-empty)  
          evaluateFormula(paf.child, state) ||
            (!state.children.isEmpty && state.children.forall { evaluateFormula(paf, _) })
        case pef: pEF =>
          //child holds in one of the current plus all children states
          state.exists { evaluateFormula(pef.child, _) }

        case pau: pAU =>
          //either right holds in the current state, or left holds in the current state and pau holds in all children states (non-empty)
          evaluateFormula(pau.right, state) ||
            (!state.children.isEmpty && evaluateFormula(pau.left, state)
              && state.children.forall { evaluateFormula(pau, _) })
        case peu: pEU =>
          //either right holds in the current state, or left holds in the current state and pau holds in one of children states (non-empty)
          evaluateFormula(peu.right, state) ||
            (!state.children.isEmpty && evaluateFormula(peu.left, state)
              && state.children.exists { evaluateFormula(peu, _) })

        case pax: pAX =>
          //child holds in all children states (in any)
          state.children.forall { evaluateFormula(pax.child, _) }
        case pex: pEX =>
          //child holds in one of the children states (at least one, which means non-empty)
          state.children.exists { evaluateFormula(pex.child, _) }

      }
    if (cacheEnabled) {
      logTrace(s"cache formula $formula")
      state.cacheFormula(formula, result)
    }
    result
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
    val variable = quantifier.variable
    if (quantifier.child.isInstanceOf[PredicateCall]) {
      //handle a special case, only one predicate
      val pred = quantifier.child.asInstanceOf[PredicateCall]
      if (quantifier.quantifiedPredicate != null) {
        //defined predicate values
        return quantifier match {
          case forall: Forall =>
            if (!variable.sort.finite) {
              false
            } else {
              val quantifiedValues = getQuantifiedValues(pred, variable, state).toSet
              val values = variable.sort.values.toSet
              values.forall { quantifiedValues.contains(_) }
            }
          case exists: Exists => !getQuantifiedValues(pred, variable, state).isEmpty
        }
      }
    }

    val values = getQuantifiedValues(quantifier, state)
    val transformed = quantifier.child.transform {
      //ignore the evaluation of the quantified predicate  
      case pred: PredicateCall if (pred == quantifier.quantifiedPredicate) => True
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
    if (quantifier.quantifiedPredicate != null) {
      getQuantifiedValues(quantifier.quantifiedPredicate, quantifier.variable, state)
    } else {
      quantifier.variable.sort.values
    }
  }

  private def getQuantifiedValues(predicate: PredicateCall, variable: Variable, state: State): Traversable[Any] = {
    val impl = context.getPredicateImpl(predicate.definition)

    val index = predicate.parameters.indexOf(variable)
    val otherParams = predicate.parameters.withFilter(_ != variable).map(evaluateParam(_, state))
    val values = impl.quantifiedValues(index, otherParams)
    //TODO: should we check values?
    values.foreach(value => {
      if (!variable.sort.validValue(value)) {
        throw new IllegalValueException(s"$value returned by ${predicate.definition.clazz} is not a valid value of ${variable.sort.nodeName} ${variable.sort.name}.")
      }
    })
    values
  }

}