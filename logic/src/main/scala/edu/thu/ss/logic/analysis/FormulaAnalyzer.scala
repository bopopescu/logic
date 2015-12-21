package edu.thu.ss.logic.analysis

import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.policy.Rule
import edu.thu.ss.logic.formula.LogicDefinitions
import edu.thu.ss.logic.util.Utils
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import scala.collection.mutable.ListBuffer
import edu.thu.ss.logic.paser.UnresolvedVariable
import scala.collection.mutable
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.AnalysisException
import org.xml.sax.helpers.NewInstance
import edu.thu.ss.logic.policy.FormulaWrapper
import edu.thu.ss.logic.formula.Quantifier
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.AnalysisException

abstract class FormulaAnalyzer extends Logging {
  def analyze(formulas: Seq[FormulaWrapper], definitions: LogicDefinitions)

  val errorMsg = "Policy contains errors. See error messages above."
}

case class FormulaExpander extends FormulaAnalyzer {
  private var definitions: LogicDefinitions = null

  private var error = false
  private var curWrapper: FormulaWrapper = null

  private val expanded = new mutable.HashSet[Symbol]

  def analyze(formulas: Seq[FormulaWrapper], definitions: LogicDefinitions) {
    this.definitions = definitions
    formulas.foreach(wrapper => {
      if (!expanded.contains(wrapper.name)) {
        curWrapper = wrapper
        wrapper.formula = expand(wrapper, new mutable.HashSet[Symbol])
        expanded.add(wrapper.name)
      }
    })
    if (error) {
      throw AnalysisException(errorMsg)
    }
  }

  private def expand(formula: Formula, reached: mutable.HashSet[Symbol]): Formula = {

    formula match {
      case symbol: Symbol => {
        val option = definitions.lookupFormula(symbol)
        if (option.isDefined) {
          val wrapper = option.get
          if (reached.contains(wrapper.name)) {
            //a cycle reference
            log.error(s"Cycle formula reference is detected in ${curWrapper.kind} ${curWrapper.name}. Please fix.")
            error = true
            return False
          }
          reached.add(wrapper.name)
          if (expanded.contains(wrapper.name)) {
            wrapper.formula
          } else {
            wrapper.formula = expand(wrapper.formula, reached)
            expanded.add(wrapper.name)
            wrapper.formula
          }
        } else {
          symbol
        }
      }
      case exist: Exists => Exists(exist.variables, expand(exist.body, reached))
      case forall: Forall => Forall(forall.variables, expand(forall.body, reached))
      case function: UnresolvedFunctionCall => {
        val params = function.parameters.map(expand(_, reached))
        UnresolvedFunctionCall(function.name, function.parameters)
      }
      case not: Not => Not(expand(not.child, reached))
      case and: And => And(expand(and.left, reached), expand(and.right, reached))
      case or: Or => Or(expand(or.left, reached), expand(or.right, reached))
      case imply: Imply => And(expand(imply.left, reached), expand(imply.right, reached))

      case t: AG => AG(expand(t.child, reached), t.length)
      case t: AF => AF(expand(t.child, reached), t.length)
      case t: AX => AX(expand(t.child, reached), t.length)
      case t: AU => AU(expand(t.left, reached), expand(t.right, reached), t.length)

      case t: EG => EG(expand(t.child, reached), t.length)
      case t: EF => EF(expand(t.child, reached), t.length)
      case t: EX => EX(expand(t.child, reached), t.length)
      case t: EU => EU(expand(t.left, reached), expand(t.right, reached), t.length)

      case t: pAG => pAG(expand(t.child, reached), t.length)
      case t: pAF => pAF(expand(t.child, reached), t.length)
      case t: pAX => pAX(expand(t.child, reached), t.length)
      case t: pAU => pAU(expand(t.left, reached), expand(t.right, reached), t.length)

      case t: pEG => pEG(expand(t.child, reached), t.length)
      case t: pEF => pEF(expand(t.child, reached), t.length)
      case t: pEX => pEX(expand(t.child, reached), t.length)
      case t: pEU => pEU(expand(t.left, reached), expand(t.right, reached), t.length)

      case const: Constant => const
      case True => True
      case False => False
    }

  }

}

case class CheckFormulaUnique extends FormulaAnalyzer {
  def analyze(formulas: Seq[FormulaWrapper], definitions: LogicDefinitions) {
    var error = false
    Utils.checkUnique(formulas, _.asInstanceOf[FormulaWrapper].name, {
      case wrapper: FormulaWrapper =>
        log.error(
          s"${wrapper.kind}'s name ${wrapper.name} has already been used somewhere else. Please choose another name.")
        error = true
    })

    formulas.foreach(wrapper => {
      val variables = new ListBuffer[Variable]
      collectFormulaVariables(wrapper.formula, variables)
      Utils.checkUnique(variables, _.asInstanceOf[Variable].name, {
        case v: Variable => {
          log.error(
            s"${v.kind}'s name ${v.name} has already been used somewhere else in ${wrapper.kind}: ${wrapper.name}. Please choose another name.")
          error = true
        }

      })
    })

    if (error) {
      throw new AnalysisException(errorMsg)
    }
  }

  private def collectFormulaVariables(formula: Formula, variables: ListBuffer[Variable]) {
    formula match {
      case quantifier: Quantifier =>
        variables ++= quantifier.variables
      case _ =>

    }
    formula.children.foreach(collectFormulaVariables(_, variables))
  }
}

case class FormulaResolver extends FormulaAnalyzer {

  private var definitions: LogicDefinitions = null

  private var curWrapper: FormulaWrapper = null

  private var error = false

  def analyze(formulas: Seq[FormulaWrapper], definitions: LogicDefinitions) {
    this.definitions = definitions
    formulas.foreach(wrapper => {
      curWrapper = wrapper
      wrapper.formula = resolve(wrapper, new mutable.HashMap[Symbol, Variable])
    })
    if (error) {
      throw new AnalysisException(errorMsg)
    }
  }

  private def resolve(formula: Formula, context: mutable.Map[Symbol, Variable]): Formula = {
    formula match {
      case quantifier: Quantifier => {
        resolveQuantifier(quantifier, context)
      }
      case function: UnresolvedFunctionCall => {
        //term here
        resolveTerm(function, context)
      }

      case symbol: Symbol => {
        //should not be here
        error = true
        log.error(s"Undefined formula $symbol in ${curWrapper.kind} ${curWrapper.name}")
        False
      }
      case True => True
      case False => False
      case not: Not => Not(resolve(not.child, context))
      case and: And => And(resolve(and.left, context), resolve(and.right, context))
      case or: Or => Or(resolve(or.left, context), resolve(or.right, context))
      case imply: Imply => And(resolve(imply.left, context), resolve(imply.right, context))

      case t: AG => AG(resolve(t.child, context), t.length)
      case t: AF => AF(resolve(t.child, context), t.length)
      case t: AX => AX(resolve(t.child, context), t.length)
      case t: AU => AU(resolve(t.left, context), resolve(t.right, context), t.length)

      case t: EG => EG(resolve(t.child, context), t.length)
      case t: EF => EF(resolve(t.child, context), t.length)
      case t: EX => EX(resolve(t.child, context), t.length)
      case t: EU => EU(resolve(t.left, context), resolve(t.right, context), t.length)

      case t: pAG => pAG(resolve(t.child, context), t.length)
      case t: pAF => pAF(resolve(t.child, context), t.length)
      case t: pAX => pAX(resolve(t.child, context), t.length)
      case t: pAU => pAU(resolve(t.left, context), resolve(t.right, context), t.length)

      case t: pEG => pEG(resolve(t.child, context), t.length)
      case t: pEF => pEF(resolve(t.child, context), t.length)
      case t: pEX => pEX(resolve(t.child, context), t.length)
      case t: pEU => pEU(resolve(t.left, context), resolve(t.right, context), t.length)
    }

  }

  private def resolveQuantifier(quantifier: Quantifier, context: mutable.Map[Symbol, Variable]): Quantifier = {
    val variables = quantifier.variables.map({
      case uvar: UnresolvedVariable => {
        val sort = definitions.lookupSort(uvar.usort)
        if (!sort.isDefined) {
          error = true
          log.error(s"Undefined sort ${uvar.usort} for ${uvar.kind} ${uvar.name} in ${curWrapper.kind} ${curWrapper.name}")
        }
        Variable(uvar.name, sort.getOrElse(null))
      }
    })
    variables.foreach(v => context.put(v.name, v))
    val body = resolve(quantifier.body, context)
    variables.foreach(v => context.remove(v.name))

    quantifier match {
      case forall: Forall => Forall(variables, body)
      case exist: Exists => Exists(variables, body)
    }

  }

  private def resolveTerm(term: Term, context: mutable.Map[Symbol, Variable]): Term = {
    term match {
      case ufunc: UnresolvedFunctionCall => {
        val params = ufunc.parameters.map(resolveTerm(_, context))

        val call = if (definitions.lookupFunction(ufunc.name).isDefined) {
          FunctionCall(definitions.lookupFunction(ufunc.name).get, params)
        } else if (definitions.lookupPredicate(ufunc.name).isDefined) {
          PredicateCall(definitions.lookupPredicate(ufunc.name).get, params)
        } else {
          error = true
          log.error(s"Undefined function ${ufunc.name} in ${curWrapper.kind} ${curWrapper.name}")
          return null
        }
        checkParameters(call)
        call
      }
      case uvar: Symbol => {
        val variable = context.get(uvar)
        variable match {
          case Some(v) => v
          case None => {
            error = true
            log.error(s"Undefined variable ${uvar} in ${curWrapper.kind} ${curWrapper.name}")
            null
          }
        }
      }
      case True => True
      case False => False
      case const: Constant => const
    }
  }

  private def checkParameters(call: BaseFunctionCall) {
    val definition = call.definition
    if (call.parameters.length != definition.parameters.size) {
      error = true
      log.error(s"Incorrect number of parameters for ${call.kind} ${definition} in ${curWrapper.kind} ${curWrapper.name}")
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
        error = true
        log.error(s"Incompatible argument for ${param.kind} ${param.name} (required: ${sort2.name}, provided: ${sort1.name}) in ${call.kind} ${call.definition}")
      }
    }

    term match {
      case func: BaseFunctionCall =>
        checkSort(func.definition.range, param.sort)
      case variable: Variable =>
        checkSort(variable.sort, param.sort)
      case _ => {
        //TODO: check for constant
      }

    }
  }

}