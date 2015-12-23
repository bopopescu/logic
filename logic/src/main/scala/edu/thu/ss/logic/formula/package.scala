
package edu.thu.ss.logic

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False
import edu.thu.ss.logic.definition.ISort

class BoolSortImpl extends ISort[Boolean] {

  val valueClass = classOf[Boolean]

  def valid(value: String): Boolean = {
    value == True.toString() || value == False.toString()
  }

  def toValue(value: String): Boolean = {
    value == True.toString()
  }

  override val finite = true

  override val values = true :: false :: Nil

}

/**
 * Predefined logic elements
 */

package object formula {

  val boolSort = new Sort("bool", classOf[BoolSortImpl])

  val True = Constant("true")

  val False = Constant("false")
}
