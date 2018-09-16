package dhr.json.parser

import java.io.Writer

object WriterConfig {

  /**
    * Write JSON in pretty-print, with each value on a separate line and an indentation of two
    * spaces.
    */
  val PRETTY_PRINT: WriterConfig = PrettyPrint.indentWithSpaces(2)

  /**
    * Write JSON in its minimal form, without any additional whitespace. This is the default.
    */
  val MINIMAL: WriterConfig = {

    val writerConfig = new WriterConfig() {
      override def createWriter(writer: Writer) = new JsonWriter(writer)
    }

    writerConfig

  }

}

abstract class WriterConfig {

  def createWriter(writer: Writer): JsonWriter

}
