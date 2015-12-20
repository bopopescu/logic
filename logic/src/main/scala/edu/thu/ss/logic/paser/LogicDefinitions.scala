package edu.thu.ss.logic.paser

import scala.collection.mutable
import edu.thu.ss.logic.formula._

class LogicDefinitions {
  private val sorts: mutable.Map[String, Sort] = new mutable.HashMap

  private val functions: mutable.Map[String, FunctionDef] = new mutable.HashMap

  private val predicates: mutable.Map[String, PredicateDef] = new mutable.HashMap

  def addSort(sort: Sort) {
    if (sorts.contains(sort.name)) {
      throw new AnalyzeException(sort.name)
    }

    sorts.put(sort.name, sort)
  }

  def addFunction(function: FunctionDef) {
    if (functions.contains(function.name)) {
      throw new AnalyzeException(function.name)
    }

    functions.put(function.name, function)
  }

  def addPredicate(predicate: PredicateDef) {
    if (predicates.contains(predicate.name)) {
      throw new AnalyzeException(predicate.name)
    }

    predicates.put(predicate.name, predicate)
  }

}