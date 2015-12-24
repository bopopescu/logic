
package edu.thu.ss.logic

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False
import edu.thu.ss.logic.definition.ISort
import java.lang.{ Boolean => JBoolean }

class BoolSortImpl extends ISort[JBoolean] {

  val valueClass = classOf[JBoolean]

  def validInput(input: String): Boolean = {
    input == True.toString() || input == False.toString()
  }

  def toValue(input: String): JBoolean = {
    input == True.toString()
  }

  protected def _validValue(value: JBoolean) = true

  override val finite = true

  override val values: Seq[JBoolean] = List(true, false)

}

/**
 * Predefined logic elements
 */

package object formula {

  val boolSort = new Sort("bool", classOf[BoolSortImpl])

  val True = Constant("true")

  val False = Constant("false")
}
