/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.sql.execution.datasources.parquet

import java.io.File

import org.apache.spark.sql.Row
import org.apache.spark.sql.test.SharedSQLContext

class ParquetInteroperabilitySuite extends ParquetCompatibilityTest with SharedSQLContext {
  test("parquet files with different physical schemas but share the same logical schema") {
    // This test case writes two Parquet files, both representing the following Catalyst schema
    //
    //   StructType(
    //     StructField(
    //       "f",
    //       ArrayType(IntegerType, containsNull = false),
    //       nullable = false))
    //
    // The first Parquet file comes with parquet-avro style 2-level LIST-annotated repeated group,
    // while the other one comes with parquet-protobuf style 1-level unannotated repeated primitive
    // field.
    withTempDir { dir =>
      import DirectParquetWriter._

      val avroStylePath = new File(dir, "avro-style").getCanonicalPath
      val protobufStylePath = new File(dir, "protobuf-style").getCanonicalPath

      val avroStyleSchema =
        """message avro_style {
          |  required group f (LIST) {
          |    repeated int32 array;
          |  }
          |}
        """.stripMargin

      writeDirect(avroStylePath, avroStyleSchema) { writer =>
        message(writer) { rc =>
          field(rc, "f") {
            group(rc) {
              field(rc, "array") {
                rc.addInteger(0)
                rc.addInteger(1)
              }
            }
          }
        }

        message(writer) { rc =>
          field(rc, "f") {
            group(rc) {
              field(rc, "array") {
                rc.addInteger(2)
                rc.addInteger(3)
              }
            }
          }
        }
      }

      logParquetSchema(avroStylePath)

      val protobufStyleSchema =
        """message protobuf_style {
          |  repeated int32 f;
          |}
        """.stripMargin

      writeDirect(protobufStylePath, protobufStyleSchema) { writer =>
        message(writer) { rc =>
          field(rc, "f") {
            rc.addInteger(4)
            rc.addInteger(5)
          }
        }

        message(writer) { rc =>
          field(rc, "f") {
            rc.addInteger(6)
            rc.addInteger(7)
          }
        }
      }

      logParquetSchema(protobufStylePath)

      checkAnswer(
        sqlContext.read.parquet(dir.getCanonicalPath),
        Seq(
          Row(Seq(0, 1)),
          Row(Seq(2, 3)),
          Row(Seq(4, 5)),
          Row(Seq(6, 7))))
    }
  }
}
