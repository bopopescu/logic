package edu.thu.ss.logic.analysis

import edu.thu.ss.logic.paser.UnresolvedDefinition
import edu.thu.ss.logic.formula.LogicDefinitions
import scala.collection.mutable
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.paser.UnresolvedSort
import edu.thu.ss.logic.formula.Sort
import edu.thu.ss.logic.paser.UnresolvedParameter
import edu.thu.ss.logic.formula.Parameter
import edu.thu.ss.logic.formula.Parameter
import edu.thu.ss.logic.paser.UnresolvedElement
import edu.thu.ss.logic.paser.UnresolvedFunctionDef
import edu.thu.ss.logic.paser.UnresolvedPredicateDef
import edu.thu.ss.logic.paser.UnresolvedParameter
import edu.thu.ss.logic.paser.UnresolvedParameter
import edu.thu.ss.logic.paser.UnresolvedPredicateDef
import edu.thu.ss.logic.paser.UnresolvedBaseFunctionDef
import edu.thu.ss.logic.paser.UnresolvedBaseFunctionDef
import edu.thu.ss.logic.paser.UnresolvedFunctionDef
import edu.thu.ss.logic.paser.UnresolvedFunctionDef
import edu.thu.ss.logic.formula.FunctionDef
import edu.thu.ss.logic.formula.PredicateDef
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.util.Utils
import edu.thu.ss.logic.paser.UnresolvedFormulaDef
import edu.thu.ss.logic.formula.FormulaDef
import edu.thu.ss.logic.paser.UnresolvedFormulaDef

abstract class DefinitionAnalyzer extends Logging {
  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions)

  protected val errorMsg = "#definition contains errors. See error messages above."
}

case class CheckDefinitionUnique extends DefinitionAnalyzer with Logging {

  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    var error = false
    val set = new mutable.HashSet[String]

    Utils.checkUnique(udefs, _.asInstanceOf[UnresolvedDefinition].name, {
      case udef: UnresolvedDefinition =>
        log.error(
          s"${udef.kind}'s name ${udef.name} has already been used somewhere else. Please choose another name.")
        error = true
    })

    udefs.withFilter(_.isInstanceOf[UnresolvedBaseFunctionDef]).foreach {
      case ufunc: UnresolvedBaseFunctionDef =>
        Utils.checkUnique(ufunc.parameters, _.asInstanceOf[UnresolvedParameter].name, {
          case uparam: UnresolvedParameter =>
            log.error(
              s"Pamarater's name ${uparam.name} has already been used somewhere else in ${ufunc.kind} ${ufunc.name}. Please choose another name.")
            error = true
        })

    }
    if (error) {
      throw AnalysisException(errorMsg)
    }

  }
}

case class ClassAnalyzer extends DefinitionAnalyzer {

  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    var error = false
    udefs.withFilter(!_.clazz.isEmpty).foreach(udef => {
      try {
        val clazz = Class.forName(udef.clazz)
      } catch {
        case t: ClassNotFoundException => {
          log.error(s"Class ${udef.clazz} is not found for ${udef.kind} ${udef.name}.")
          error = true
        }
      }
    })
    if (error) {
      throw AnalysisException(errorMsg)

    }
  }
}

case class SortResolver extends DefinitionAnalyzer with Logging {
  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    udefs.withFilter(_.isInstanceOf[UnresolvedSort]).foreach(usort => {
      val clazz = Class.forName(usort.clazz)
      val sort = Sort(usort.name, clazz)
      definitions.addSort(sort)
    })
  }
}

abstract class BaseFunctionDefResolver extends DefinitionAnalyzer {
  protected def resolveParameters(udef: UnresolvedDefinition, uparams: Seq[UnresolvedParameter], definitions: LogicDefinitions): Option[Seq[Parameter]] = {
    var error = false
    val params = uparams.map(uparam => {
      val sort = definitions.lookupSort(uparam.sort)
      sort match {
        case Some(s) => {
          Parameter(uparam.name, s)
        }
        case None => {
          log.error(s"Undefined sort ${uparam.sort} for parameter ${uparam.name} in ${udef.kind} ${udef.name}.")
          error = true
          null
        }
      }
    })
    if (!error) {
      Some(params)
    } else {
      None
    }
  }

}

case class FunctionDefResolver extends BaseFunctionDefResolver {

  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    var error = false
    udefs.withFilter(_.isInstanceOf[UnresolvedFunctionDef]).foreach({
      case ufunc: UnresolvedFunctionDef => {
        val clazz = Class.forName(ufunc.clazz)
        val params = resolveParameters(ufunc, ufunc.parameters, definitions)
        val range = definitions.lookupSort(ufunc.range)
        if (!params.isDefined || !range.isDefined) {
          error = true
          if (!range.isDefined) {
            log.error(s"Undefined sort ${ufunc.range} for the range of ${ufunc.kind} ${ufunc.name}.")
          }
        } else {
          val func = FunctionDef(ufunc.name, params.get, range.get, clazz)
          definitions.addFunction(func)
        }
      }
    })

  }

}

case class PredicateDefResolver extends BaseFunctionDefResolver {
  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    var error = false
    udefs.withFilter(_.isInstanceOf[UnresolvedPredicateDef]).foreach({
      case upred: UnresolvedPredicateDef => {
        val clazz = Class.forName(upred.clazz)
        val params = resolveParameters(upred, upred.parameters, definitions)
        if (!params.isDefined) {
          error = true
        } else {
          val pred = PredicateDef(upred.name, params.get, clazz)
          definitions.addPredicate(pred)
        }
      }
    })

  }
}

case class FormulaDefResolver extends DefinitionAnalyzer {
  def analyze(udefs: Seq[UnresolvedDefinition], definitions: LogicDefinitions) {
    udefs.withFilter(_.isInstanceOf[UnresolvedFormulaDef]).foreach({
      case udef: UnresolvedFormulaDef =>
        definitions.addFormula(FormulaDef(udef.name, udef.formula))

    })
  }

}