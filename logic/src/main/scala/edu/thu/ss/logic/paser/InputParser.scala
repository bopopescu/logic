package edu.thu.ss.logic.paser

import scala.util.parsing.combinator.JavaTokenParsers
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import edu.thu.ss.logic.policy.Policy
import java.io.StringReader

/**
 * a high-level parser to handle input file.
 */
class InputParser {

  def parseFile(path: String): Policy = {

    val file = new File(path)
    val reader = new BufferedReader(new FileReader(file))
    null
  }

}