package dhr.json.parser

import java.io.Writer

object PrettyPrint {

  /**
    * Print every value on a separate line. Use tabs (<code>\t</code>) for indentation.
    *
    * @return A PrettyPrint instance for wrapped mode with tab indentation
    */
  def singleLine(): PrettyPrint= new PrettyPrint(null)

  /**
    * Print every value on a separate line. Use the given number of spaces for indentation.
    *
    * @param number
    *          the number of spaces to use
    * @return A PrettyPrint instance for wrapped mode with spaces indentation
    */
  def indentWithSpaces(number:Int): PrettyPrint =  {
    require(number >= 0, "number is negative")

    val chars:Array[Char] = Array.fill(number)(' ')

    new PrettyPrint(chars)
  }

  /**
    * Do not break lines, but still insert whitespace between values.
    *
    * @return A PrettyPrint instance for single-line mode
    */
  def indentWithTabs(): PrettyPrint = {
    new PrettyPrint(Array.fill(1)('\t'));
  }

  private class PrettyPrintWriter(writer: Writer, indentChars: Array[Char]) extends JsonWriter(writer:Writer) {

    var indent:Int = 0

    override def writeArrayOpen(): Unit = {
      indent += 1
      writer.write('[')
      writeNewLine
    }

    override def writeArrayClose(): Unit = {
      indent -= 1
      writeNewLine
      writer.write(']')
    }

    override def writeArraySeperator(): Unit = {
      writer.write(',')
      if (!writeNewLine()) {
        writer.write(' ')
      }
    }

    override def writeObjectOpen(): Unit = {
      indent += 1
      writer.write('{')
      writeNewLine()
    }

    override def writeObjectClose(): Unit = {
      indent -= 1
      writeNewLine()
      writer.write('}')
    }

    override def writeMemberSeperator(): Unit = {
      writer.write(':')
      writer.write(' ')
    }

    override def writeObjectSeperator(): Unit = {
      writer.write(',')
      if (!writeNewLine()) {
        writer.write(' ')
      }
    }

    private def writeNewLine(): Boolean = {
      if (indentChars == null) {
        return false
      }
      writer.write('\n')
      for (a <- 0 to indent) {
        writer.write(indentChars)
      }
      return true
    }

  }

}

class PrettyPrint(indentChars: Array[Char]) extends WriterConfig {

  import PrettyPrint._

  override def createWriter(writer: Writer): JsonWriter = new PrettyPrintWriter(writer, indentChars)

}
