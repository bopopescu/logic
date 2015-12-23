package edu.thu.ss.logic.formula

trait TemporalFormula {
  def length: Int

}

/**
 * temporal part
 */

case class AG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "AG"

}

case class AF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "AF"

}

case class AX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "AX"

}

case class AU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val kind = "AU"

}

case class EG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "EG"

}

case class EF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "EF"

}

case class EX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "EX"

}

case class EU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val kind = "EU"

}

/**
 * temporal part
 */

case class pAG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pAG"

}

case class pAF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pAF"

}

case class pAX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pAX"

}

case class pAU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val kind = "pAU"

}

case class pEG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pEG"

}

case class pEF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pEF"

}

case class pEX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  val kind = "pEX"

}

case class pEU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  val kind = "pEU"

}

