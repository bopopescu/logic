package edu.thu.ss.logic.formula

import sun.security.util.Length

trait TemporalFormula {
  def length: Int

}

/**
 * temporal part
 */

case class AG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "AG"

}

case class AF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "AF"

}

case class AX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "AX"

}

case class AU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def getName = "AU"

}

case class EG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "EG"

}

case class EF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "EF"

}

case class EX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "EX"

}

case class EU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def getName = "EU"

}

/**
 * temporal part
 */

case class pAG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pAG"

}

case class pAF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pAF"

}

case class pAX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pAX"

}

case class pAU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def getName = "pAU"

}

case class pEG(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pEG"

}

case class pEF(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pEF"

}

case class pEX(child: Formula, length: Int = 0) extends UnaryFormula with TemporalFormula {
  override def getName = "pEX"

}

case class pEU(left: Formula, right: Formula, length: Int = 0) extends BinaryFormula with TemporalFormula {
  override def getName = "pEU"

}

