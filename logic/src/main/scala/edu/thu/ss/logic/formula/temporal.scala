package edu.thu.ss.logic.formula

trait TemporalFormula {

}

/**
 * temporal part
 */

case class AG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AG"

}

case class AF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AF"

}

case class AX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "AX"

}

case class AU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "AU"

}

case class EG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EG"

}

case class EF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EF"

}

case class EX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "EX"

}

case class EU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "EU"

}

/**
 * temporal part
 */

case class pAG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAG"

}

case class pAF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAF"

}

case class pAX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pAX"

}

case class pAU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "pAU"

}

case class pEG(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEG"

}

case class pEF(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEF"

}

case class pEX(child: Formula) extends UnaryFormula with TemporalFormula {
  val nodeName = "pEX"

}

case class pEU(left: Formula, right: Formula) extends BinaryFormula with TemporalFormula {
  val nodeName = "pEU"

}

