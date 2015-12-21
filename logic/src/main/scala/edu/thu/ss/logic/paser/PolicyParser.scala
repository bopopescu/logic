package edu.thu.ss.logic.paser

import scala.annotation.migration
import edu.thu.ss.logic.analysis.CheckDefinitionUnique
import edu.thu.ss.logic.analysis.CheckFormulaUnique
import edu.thu.ss.logic.analysis.ClassAnalyzer
import edu.thu.ss.logic.analysis.DefinitionAnalyzer
import edu.thu.ss.logic.analysis.FormulaAnalyzer
import edu.thu.ss.logic.analysis.FormulaDefResolver
import edu.thu.ss.logic.analysis.FormulaResolver
import edu.thu.ss.logic.analysis.FunctionDefResolver
import edu.thu.ss.logic.analysis.PredicateDefResolver
import edu.thu.ss.logic.analysis.SortResolver
import edu.thu.ss.logic.policy.FormulaWrapper
import edu.thu.ss.logic.policy.Policy
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.formula.LogicDefinitions
import edu.thu.ss.logic.analysis.FormulaExpander

/**
 * a high-level parser to handle input file.
 */
class PolicyParser extends Logging {

  def parsePolicy(path: String): Policy = {
    val (definitionList, ruleList) = LogicParser.parseFile(path)

    val definitions: LogicDefinitions = parseDefinitions(definitionList)

    //    parseFormulas(definitions.getFormulas.values.toSeq, definitions)
    parseFormulas(ruleList, definitions)
    new Policy(definitions, ruleList)
  }

  private def parseDefinitions(list: Seq[UnresolvedDefinition]): LogicDefinitions = {
    val analyzers: Seq[DefinitionAnalyzer] =
      CheckDefinitionUnique() ::
        ClassAnalyzer() ::
        SortResolver() ::
        FunctionDefResolver() ::
        PredicateDefResolver() ::
        FormulaDefResolver() ::
        Nil

    val definitions = new LogicDefinitions
    analyzers.foreach(_.analyze(list, definitions))
    definitions
  }

  private def parseFormulas(list: Seq[FormulaWrapper], definitions: LogicDefinitions) {
    val analyzers: Seq[FormulaAnalyzer] =
      FormulaExpander() ::
        CheckFormulaUnique() ::
        FormulaResolver() ::
        Nil
    analyzers.foreach(_.analyze(list, definitions))
  }

}