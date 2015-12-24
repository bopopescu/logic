package edu.thu.ss.logic.evaluation

import org.scalatest.FunSuite

import edu.thu.ss.logic.paser.PolicyParser

class FormulaEvaluatorSuite extends FunSuite {

  val parser = new PolicyParser
  val evaluator = new FormulaEvaluator(new EvaluationContext)

  test("test1") {
    val policy = parser.parsePolicy("policy/policy1")
    policy.rules.foreach { rule =>
      {
        val value = evaluator.evaluate(rule)
        println(s"$rule = $value")
      }
    }
  }

}