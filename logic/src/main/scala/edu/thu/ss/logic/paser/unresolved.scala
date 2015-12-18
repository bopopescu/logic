package edu.thu.ss.logic.paser

import edu.thu.ss.logic.formula.Formula
import edu.thu.ss.logic.formula.Symbol
import edu.thu.ss.logic.formula.Term

trait UnresolvedElement {
  def symbol: Symbol

}

case class UnresolvedSort(symbol: Symbol) extends UnresolvedElement {
  override def toString = symbol.toString

}

object UnknownSort extends UnresolvedSort(Symbol("Unknown")) {

}

case class UnresolvedFunctionCall(symbol: Symbol, parameters: Seq[Term]) extends Term with UnresolvedElement {

  override def getName = symbol.toString

  override def toString = s"$getName(${parameters.mkString(", ")})"
}

case class UnresolvedVariable(symbol: Symbol, sort: UnresolvedSort) extends Term with UnresolvedElement {
  override def getName = "VARIABLE"

  override def toString = symbol.toString
}

abstract class UnresolvedQuantifier extends Formula with UnresolvedElement {
  def variables: Seq[UnresolvedVariable]
  def body: Formula

  override def children: Seq[Formula] = body :: Nil

  override def symbol: Symbol = null

  override def toString: String = {
    val variableStr = variables.map(v => s"${v.symbol} ${v.sort}").mkString(",")
    s"$getName ${variableStr}.($body)"
  }

}

case class UnresolvedForall(variables: Seq[UnresolvedVariable], body: Formula) extends UnresolvedQuantifier {
  override def getName = "FORALL"

}

case class UnresolvedExists(variables: Seq[UnresolvedVariable], body: Formula) extends UnresolvedQuantifier {
  override def getName = "EXISTS"

}
