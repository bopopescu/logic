package edu.thu.ss.logic.formula

import org.scalatest.FunSuite
import edu.thu.ss.logic.paser.FormulaParser

class FormulaParserTest extends FunSuite {

  val parser = new FormulaParser

  test("test1") {
    val formula = parser.parse("f1() AND f2(1)").get
    println(formula)
  }

  test("test2") {
    val formula = parser.parse("f1() AND f2(1) AND f3(f1())").get
    println(formula)
  }

  test("test3") {
    val formula = parser.parse("f1() OR f2(1) AND f3(f1())").get
    println(formula)
  }

  test("test ()") {
    val formula = parser.parse("(f1() OR f2(1)) AND f3(f1())").get
    println(formula)
  }

  test("test quantifier1") {
    val formula = parser.parse("forall x int, y int. f(x) AND g(y)").get
    println(formula)
  }

  test("test quantifier2") {
    val formula = parser.parse("forall x int. exists y int. f(x) AND g(y)").get
    println(formula)
  }

  test("test quantifier3") {
    val formula = parser.parse("(forall x int. f(x)) AND g(y)").get
    println(formula)
  }

  test("test temporal") {
    val formula = parser.parse("AG( f1 AND AF g())").get
    println(formula)
  }

}