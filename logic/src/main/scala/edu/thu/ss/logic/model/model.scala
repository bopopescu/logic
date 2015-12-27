package edu.thu.ss.logic.model

import edu.thu.ss.logic.tree.DoubleNode
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import scala.collection.mutable.ListBuffer
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import org.apache.spark.sql.catalyst.plans.logical.BinaryNode
import org.apache.spark.sql.catalyst.plans.logical.UnaryNode
import org.apache.spark.sql.catalyst.plans.logical.LeafNode

abstract class State extends DoubleNode[State] {
  var parent: State = null

  def nodeName = "state"

  def plan: LogicalPlan

}

case class BinaryState(left: State, right: State, plan: BinaryNode) extends State {

  val children = left :: right :: Nil

}

case class UnaryState(child: State, plan: UnaryNode) extends State {

  val children = child :: Nil
}

case class LeafState(plan: LeafNode) extends State {

  val children = Nil

}

case class QueryModel(initialStates: Seq[State], finalState: State) {

}

object QueryModel {
  val Empty = {
    val state = new LeafState(null)
    new QueryModel(Seq(state), state)
  }

  def fromQueryPlan(plan: LogicalPlan): QueryModel = {
    val initialStates = new ListBuffer[State]
    val finalState = translatePlan(plan, initialStates)

    new QueryModel(initialStates, finalState)
  }

  private def translatePlan(plan: LogicalPlan, initialStates: ListBuffer[State]): State = {
    plan match {
      case binary: BinaryNode =>
        val left = translatePlan(binary.left, initialStates)
        val right = translatePlan(binary.right, initialStates)
        val state = BinaryState(left, right, binary)
        left.parent = state
        right.parent = state
        state
      case unary: UnaryNode =>
        val child = translatePlan(unary.child, initialStates)
        val state = UnaryState(child, unary)
        child.parent = state
        state
      case leaf: LeafNode =>
        val state = LeafState(leaf)
        initialStates.append(state)
        state
    }
  }

}