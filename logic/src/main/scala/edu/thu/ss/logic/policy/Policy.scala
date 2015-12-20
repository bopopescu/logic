package edu.thu.ss.logic.policy

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser.LogicDefinitions
import scala.collection.mutable

case class Rule(id: String, formula: Formula) {

  override def toString = s"$id: $formula"
}

class Policy(val definitions: LogicDefinitions) {
  val rules: mutable.Seq[Rule] = new mutable.ListBuffer

}