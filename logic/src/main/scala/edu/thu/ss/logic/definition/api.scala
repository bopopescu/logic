package edu.thu.ss.logic.definition

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False
import java.{ lang => java }

trait IDefinition {

}

trait ISort[T] extends IDefinition {

  val valueClass: Class[T]

  def validInput(input: String): Boolean

  def parseInput(input: String): T

  def validValue(value: Any): Boolean = {
    try {
      _validValue(value.asInstanceOf[T])
    } catch {
      case t: ClassCastException =>
        t.printStackTrace()
        false
    }
  }

  def values: Traversable[T] = throw new UnsupportedOperationException

  def finite: Boolean = false

  protected def _validValue(value: T): Boolean

}

trait IBaseFunction {
  //TODO add node
}

trait IFunction extends IBaseFunction {

}

trait IPredicate extends IBaseFunction {

  def finite(index: Int): Boolean = false

  def quantifiedValues(index: Int, otherParams: Seq[Any]): Seq[Any] = throw new UnsupportedOperationException

}

