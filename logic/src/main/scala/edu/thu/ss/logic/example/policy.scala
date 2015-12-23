package edu.thu.ss.logic.example

import edu.thu.ss.logic.definition.ISort
import edu.thu.ss.logic.definition.IFunction
import edu.thu.ss.logic.definition.IPredicate
import scala.reflect.runtime.universe._
import edu.thu.ss.logic.formula.Sort

class IntSort extends ISort[Int] {
  val valueClass = classOf[Int]

  def valid(value: String): Boolean = {
    try {
      value.toInt
    } catch {
      case _: NumberFormatException => false
    }
    true
  }

  def toValue(value: String): Int = value.toInt
}

class ColumnSort extends ISort[String] {
  val valueClass = classOf[String]

  def valid(value: String): Boolean = true

  def toValue(value: String): String = value
}

class Add extends IFunction {
  def evaluate(x: Int, y: Int): Int = x.toInt + y
}

class IsZero extends IPredicate {
  def evaluate(x: Int): Boolean = x == 0

}

class Equals extends IPredicate {
  def evaluate(x: Int, y: Int): Boolean = x == y
}

class IsTrue extends IPredicate {
  def evaluate(value: Boolean): Boolean = value == true
}

class Output extends IPredicate {
  def evaluate(column: String): Boolean = true

}