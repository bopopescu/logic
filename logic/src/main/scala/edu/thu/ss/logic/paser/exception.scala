package edu.thu.ss.logic.paser

case class ParseException(msg: String) extends Exception(msg) {
}

case class AnalysisException(msg: String) extends Exception(msg) {
}
