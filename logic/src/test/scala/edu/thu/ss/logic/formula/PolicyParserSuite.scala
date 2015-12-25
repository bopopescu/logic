package edu.thu.ss.logic.formula

import org.scalatest.FunSuite
import edu.thu.ss.logic.paser.LogicParser
import java.io.StringReader
import scala.util.parsing.combinator.Parsers
import java.io.FileReader
import edu.thu.ss.logic.paser.PolicyParser
import edu.thu.ss.logic.evaluation.EvaluationContext
import edu.thu.ss.logic.evaluation.FormulaEvaluator
import edu.thu.ss.logic.example.IntSort
import edu.thu.ss.logic.definition.ISort
import scala.reflect.runtime.universe._

class PolicyParserTest extends FunSuite {

  val parser = new PolicyParser

  test("test1") {
    val policy = parser.parsePolicy("policy/policy1")
    println(policy)
  }

  test("test2") {

  }

}