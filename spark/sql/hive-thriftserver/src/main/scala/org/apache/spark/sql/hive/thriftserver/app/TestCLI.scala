package org.apache.spark.sql.hive.thriftserver.app

import org.apache.spark.sql.hive.thriftserver.SparkSQLCLIDriver
import scala.math.BigDecimal

/**
 * start spark-sql in eclipse
 */
object TestCLI extends App {
  System.setProperty("spark.master", "local");
  SparkSQLCLIDriver.main(args);
}



