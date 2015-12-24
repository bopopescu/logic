package edu.thu.ss.logic.example

import edu.thu.ss.logic.definition.ISort
import edu.thu.ss.logic.definition.IFunction
import edu.thu.ss.logic.definition.IPredicate
import edu.thu.ss.logic.formula.Sort
import edu.thu.ss.logic.util.LogicUtils
import java.lang.{ Integer => JInt, Boolean => JBoolean }

class IntSort extends ISort[JInt] {
  val valueClass = classOf[JInt]

  def validInput(input: String): Boolean = LogicUtils.isInt(input)

  def toValue(value: String): JInt = value.toInt

  def _validValue(value: JInt): Boolean = true
}

class ColumnSort extends ISort[String] {
  val valueClass = classOf[String]

  def validInput(value: String): Boolean = true

  def toValue(value: String): String = value

  def _validValue(value: String): Boolean = true
}

class Add extends IFunction {
  def evaluate(x: JInt, y: JInt): JInt = x + y
}

class IsZero extends IPredicate {
  def evaluate(x: JInt): JBoolean = x == 0

}

class Equals extends IPredicate {
  def evaluate(x: JInt, y: JInt): JBoolean = x == y
}

class IsTrue extends IPredicate {
  def evaluate(value: JBoolean): JBoolean = value == true
}

class Output extends IPredicate {
  def evaluate(column: String): JBoolean = true

}