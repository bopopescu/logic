package edu.thu.ss.logic.util

import scala.collection.mutable
import scala.reflect.runtime.universe._
import edu.thu.ss.logic.example.IntSort
import edu.thu.ss.logic.definition.ISort
import java.{ lang => java }

object LogicUtils {

  val mirror = runtimeMirror(getClass.getClassLoader)

  private val numerics = List(classOf[Int], classOf[Short], classOf[Double], classOf[Float],
    classOf[Long], classOf[Byte], classOf[Boolean], classOf[java.Number], classOf[java.Boolean])

  def isNumericalValue(value: Any): Boolean = {
    numerics.exists { _.isAssignableFrom(value.getClass) }
  }

  private def isNumerical(value: String, test: String => Any): Boolean = {
    try {
      test(value)
      true
    } catch {
      case e: NumberFormatException => false
    }
  }

  def isNumerical(value: String): Boolean =
    isNumerical(value, _.toDouble)

  def isInt(value: String): Boolean =
    isNumerical(value, _.toInt)

  def checkUnique(seq: Seq[Any], toId: (Any) => String, onError: (Any) => Unit) {
    val set = new mutable.HashSet[String]
    seq.foreach(ele => {
      val id = toId(ele)
      if (set.contains(id)) {
        onError(ele)
      } else {
        set.add(id)
      }
    })
  }

}