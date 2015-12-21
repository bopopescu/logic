package edu.thu.ss.logic.formula

import org.scalatest.FunSuite
import edu.thu.ss.logic.paser.LogicParser
import java.io.StringReader
import scala.util.parsing.combinator.Parsers
import java.io.FileReader

class LogicParserTest extends FunSuite {

  val parser = LogicParser
  implicit def toReader(str: String) = {
    new parser.lexical.Scanner(str)
  }

  test("test1") {
    parseFormula("f1() AND f2(1)");
  }

  test("test2") {
    parseFormula("f1() AND f2(1) AND f3(f1())")
  }

  test("test3") {
    parseFormula("f1() OR f2(1) AND f3(f1())")
  }

  test("test associativity") {
    parseFormula("f1() IMPLY f2() IMPLY f3() IMPLY f4()")

  }

  test("test ()") {
    parseFormula("(f1() OR f2(1)) AND f3(f1())")

  }

  test("test quantifier1") {
    parseFormula("forall int x, int y. f(x) AND g(y)")

  }

  test("test quantifier2") {
    parseFormula("forall int x. exists int y. f(x) AND g(y)")

  }

  test("test quantifier3") {
    parseFormula("(forall int x. f(x)) AND g(y)")

  }

  test("test temporal") {
    parseFormula("AF ( f1() AND AF g())")

  }

  test("test symbol") {
    parseFormula("f1() && f2() || g1() -> f3()")

  }

  test("test sort") {
    parseDefinition("define sort int class=edu.thu.ss.logic.IntSort;")
  }
  test("test function") {
    parseDefinition("define function int add(int x, double y) class=edu.thu.ss.logic.Add;")
  }
  test("test predicate") {
    parseDefinition("define predicate isZero(int x) class=edu.thu.ss.logic.isZero;")
  }

  test("test file") {
    val (defs, rules) = parser.parseFile("policy/policy1")
    defs.foreach(println(_))
    rules.foreach(println(_))
  }

  def parseFormula(str: String) {
    val result = parser.parseFormula(str)
    if (result.successful) {
      println(result.get)
    } else {
      fail(result.toString)
    }
  }

  def parseDefinition(str: String) {
    val result = parser.parseDef(str)
    if (result.successful) {
      println(result.get)
    } else {
      fail(result.toString)
    }
  }

}