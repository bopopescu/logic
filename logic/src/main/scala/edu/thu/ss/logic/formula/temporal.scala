package edu.thu.ss.logic.formula

import sun.security.util.Length

trait TemporalFormula {
  def length: Int

}

/**
 * temporal part
 */

case class AG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "AG"

}

case class AF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "AF"

}

case class AX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "AX"

}

case class AU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def kind = "AU"

}

case class EG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "EG"

}

case class EF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "EF"

}

case class EX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "EX"

}

case class EU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def kind = "EU"

}

/**
 * temporal part
 */

case class pAG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pAG"

}

case class pAF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pAF"

}

case class pAX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pAX"

}

case class pAU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def kind = "pAU"

}

case class pEG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pEG"

}

case class pEF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pEF"

}

case class pEX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def kind = "pEX"

}

case class pEU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def kind = "pEU"

}

