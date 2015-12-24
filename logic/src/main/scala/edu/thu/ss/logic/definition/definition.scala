package edu.thu.ss.logic.definition

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False

trait IDefinition {

}

trait ISort[T <: AnyRef] extends IDefinition {

  val valueClass: Class[T]

  def validInput(input: String): Boolean

  def validValue(value: Any): Boolean = {
    try {
      _validValue(valueClass.cast(value))
    } catch {
      case t: ClassCastException =>
        t.printStackTrace()
        false
    }
  }

  def toValue(input: String): T

  def values: Seq[T] = throw new UnsupportedOperationException

  def finite: Boolean = false

  protected def _validValue(value: T): Boolean

}

trait IBaseFunction {
}

trait IFunction extends IBaseFunction {

}

trait IPredicate extends IBaseFunction {

}
