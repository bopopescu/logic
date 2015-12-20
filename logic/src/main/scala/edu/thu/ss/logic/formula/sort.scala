package edu.thu.ss.logic.formula

case class Sort(name: Symbol, clazz: Class[_]) extends LogicDefinition {
  override def toString = name.toString
}

object BoolSort extends Sort("Bool", classOf[Any]) {

}
