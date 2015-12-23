package edu.thu.ss.logic.util

import scala.collection.mutable
import scala.reflect.runtime.universe._
import edu.thu.ss.logic.example.IntSort
import edu.thu.ss.logic.definition.ISort

object LogicUtils {

  val mirror = runtimeMirror(getClass.getClassLoader)

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

  def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

  def getParameterizedType(obj: Any, clazz: Class[_]): Class[_] = {
    val objTag = getTypeTag(obj)

    val symbol = mirror.staticClass(clazz.getName)

    val TypeRef(_, _, params) = objTag.tpe.baseType(symbol)
    if (params.length == 0) {
      throw new IllegalArgumentException
    }
    mirror.runtimeClass(params.head)

  }

}