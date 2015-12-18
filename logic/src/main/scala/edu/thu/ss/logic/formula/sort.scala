package edu.thu.ss.logic.formula

case class Sort(name: Symbol) {

  override def toString = name.toString
}

object BoolSort extends Sort(Symbol("Bool")) {

}
