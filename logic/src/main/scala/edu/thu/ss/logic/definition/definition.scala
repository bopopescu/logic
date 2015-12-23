package edu.thu.ss.logic.definition

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False

trait IDefinition {

}

trait ISort[T] extends IDefinition {

  val valueClass: Class[T]

  def valid(value: String): Boolean

  def toValue(value: String): T

  def values: Seq[T] = throw new UnsupportedOperationException

  def finite: Boolean = false

}

trait IBaseFunction {
}

trait IFunction extends IBaseFunction {

}

trait IPredicate extends IBaseFunction {

}
