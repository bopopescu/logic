package edu.thu.ss.logic.formula

trait TemporalFormula {
  def length: Int

}

/**
 * temporal part
 */

case class AG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "AG"

}

case class AF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "AF"

}

case class AX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "AX"

}

case class AU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val nodeName = "AU"

}

case class EG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "EG"

}

case class EF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "EF"

}

case class EX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "EX"

}

case class EU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val nodeName = "EU"

}

/**
 * temporal part
 */

case class pAG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAG"

}

case class pAF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAF"

}

case class pAX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAX"

}

case class pAU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val nodeName = "pAU"

}

case class pEG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEG"

}

case class pEF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEF"

}

case class pEX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEX"

}

case class pEU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val nodeName = "pEU"

}

