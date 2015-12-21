package edu.thu.ss.logic.util

import scala.collection.mutable

object Utils {

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