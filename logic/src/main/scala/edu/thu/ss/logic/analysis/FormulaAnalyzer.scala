package edu.thu.ss.logic.analysis

import scala.collection.mutable
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser.{ UnresolvedFunctionCall, UnresolvedVariable }
import edu.thu.ss.logic.util.LogicUtils

abstract class FormulaAnalyzer extends Analyzer[NamedFormula] {

  protected val errorMsg = "#policy contains errors. See error messages above."

}

abstract class SequentialAnalyzer extends FormulaAnalyzer {
  protected var curFormula: NamedFormula = null

  protected def analyze(formulas: Seq[NamedFormula]) {
    formulas.foreach(formula => {
      curFormula = formula
      analyzeFormula()
    })
  }

  protected def analyzeFormula()
}

case class FormulaExpander() extends SequentialAnalyzer {

  private val expanded = new mutable.HashSet[Symbol]

  override protected def analyzeFormula() {
    curFormula.formula = expand(curFormula, new mutable.HashSet[Symbol])
    expanded.add(curFormula.name)
  }

  private def expand(formula: Formula, reached: mutable.HashSet[Symbol]): Formula = {
    formula.transform({
      case symbol: Symbol => {
        definitions.lookupFormula(symbol) match {
          case Some(f) if (reached.contains(f.name)) => {
            setError(s"${curFormula.kind} ${curFormula.name} contains cycle formula reference. Please fix.")
          }
          case Some(f) =>
            reached.add(f.name)
            if (!expanded.contains(f.name)) {
              f.formula = expand(f.formula, reached)
              expanded.add(f.name)
            }
            f.formula
          case None => symbol
        }
      }
    })
  }
}

case class CheckFormulaUnique() extends FormulaAnalyzer {
  protected def analyze(formulas: Seq[NamedFormula]) {
    LogicUtils.checkUnique(formulas, _.asInstanceOf[NamedFormula].name, {
      case formula: NamedFormula =>
        setError(
          s"${formula.kind}'s name ${formula.name} has already been used somewhere else. Please choose another name.")
    })

    formulas.foreach(formula => {
      val variables = formula.flatMap({
        case quantifier: Quantifier => quantifier.variables
        case _ => Nil
      })
      LogicUtils.checkUnique(variables, _.asInstanceOf[Variable].name, {
        case v: Variable => {
          setError(
            s"${v.kind}'s name ${v.name} has already been used somewhere else in ${formula.kind}: ${formula.name}. Please choose another name.")
        }

      })
    })
  }

}

case class FormulaResolver() extends SequentialAnalyzer {

  private val context = new mutable.HashMap[Symbol, Variable]

  def analyzeFormula() {
    context.clear
    curFormula.formula = resolve(curFormula)
  }

  private def resolve(formula: Formula): Formula = {
    formula.transformDown({
      case quantifier: Quantifier => {
        resolveQuantifier(quantifier)
      }
      case ufunc: UnresolvedFunctionCall => {
        val call = resolveTerm(ufunc)
        call match {
          case func: FunctionCall => {
            //only predicate is allowed
            setError(s"${func.kind} ${func} is not a predicate.")
          }
          case _ => call
        }
      }
      case symbol: Symbol => {
        val variable = context.get(symbol)
        variable match {
          case Some(v) if v.sort != boolSort => {
            setError(s"${v.sort} ${v.kind} ${v.name} is not a ${boolSort.name} ${v.kind}")
          }
          case Some(v) => v
          case None =>
            setError(s"Undefined variable ${symbol} in ${curFormula.kind} ${curFormula.name}")
        }
      }
    }, {
      //remove variables from context
      case quantifier: Quantifier =>
        quantifier.variables.foreach { v => context.remove(v.name) }
    })

  }

  private def resolveQuantifier(quantifier: Quantifier): Quantifier = {
    val variables = quantifier.variables.map({
      case uvar: UnresolvedVariable => {
        definitions.lookupSort(uvar.usort) match {
          case Some(sort) => Variable(uvar.name, sort)
          case None =>
            setError(s"Undefined sort ${uvar.usort} for ${uvar.kind} ${uvar.name} in ${curFormula.kind} ${curFormula.name}")
            Variable(uvar.name, null)
        }
      }
    })
    variables.foreach(v => context.put(v.name, v))
    quantifier match {
      case forall: Forall => Forall(variables, forall.body)
      case exist: Exists => Exists(variables, exist.body)
    }
  }

  private def resolveTerm(term: Term): Term = {
    term match {
      case ufunc: UnresolvedFunctionCall => {
        val params = ufunc.parameters.map(resolveTerm(_))
        val call = if (definitions.lookupFunction(ufunc.name).isDefined) {
          FunctionCall(definitions.lookupFunction(ufunc.name).get, params)
        } else if (definitions.lookupPredicate(ufunc.name).isDefined) {
          PredicateCall(definitions.lookupPredicate(ufunc.name).get, params)
        } else {
          setError(s"Undefined function ${ufunc.name} in ${curFormula.kind} ${curFormula.name}")
        }
        checkParameters(call)
        call
      }
      case uvar: Symbol => {
        context.get(uvar) match {
          case Some(v) => v
          case None =>
            setError(s"Undefined variable ${uvar} in ${curFormula.kind} ${curFormula.name}")
        }
      }
      case const: Constant => const
    }
  }

  private def checkParameters(call: BaseFunctionCall) {
    val definition = call.definition
    if (call.parameters.length != definition.parameters.size) {
      setError(s"Incorrect number of parameters for ${call.kind} ${definition} in ${curFormula.kind} ${curFormula.name}")
      return
    }

    var i = 0
    while (i < call.parameters.length) {
      val term = call.parameters(i)
      val param = definition.parameters(i)
      checkParameter(term, param, call)
      i += 1
    }
  }

  private def checkParameter(term: Term, param: Parameter, call: BaseFunctionCall) {
    def checkSort(sort1: Sort, sort2: Sort) {
      if (sort1 != sort2) {
        setError(s"Incompatible argument for ${param.kind} ${param.name} (expected: ${sort2.name}, provided: ${sort1.name}) in ${call.kind} ${call.definition} for ${curFormula.kind} ${curFormula.name}")
      }
    }

    term match {
      case func: BaseFunctionCall =>
        checkSort(func.definition.range, param.sort)
      case variable: Variable =>
        checkSort(variable.sort, param.sort)

      case const: Constant => if (!param.sort.valid(const.value)) {
        setError(s"${const.value} is not a valid value of ${param.sort.kind} ${param.sort.name} in ${call} of ${curFormula.kind} ${curFormula.name}.")
      }

    }
  }
}

