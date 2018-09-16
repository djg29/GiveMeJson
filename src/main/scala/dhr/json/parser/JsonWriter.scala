package dhr.json.parser

import java.io.Writer

object JsonWriter {

  private val CONTROL_CHARACTERS_END = 0x001f

  private val QUOT_CHARS = Array('\\', '"')
  private val BS_CHARS = Array('\\', '\\')
  private val LF_CHARS = Array('\\', 'n')
  private val CR_CHARS = Array('\\', 'r')
  private val TAB_CHARS = Array('\\', 't')
  // In JavaScript, U+2028 and U+2029 characters count as line endings and must be encoded.
  // http://stackoverflow.com/questions/2965293/javascript-parse-error-on-u2028-unicode-character
  private val UNICODE_2028_CHARS = Array('\\', 'u', '2', '0', '2', '8')
  private val UNICODE_2029_CHARS = Array('\\', 'u', '2', '0', '2', '9')
  private val HEX_DIGITS = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  def getReplacementChars(ch:Char): Option[Seq[Char]] = {

    ch match {
      case c if (c > '\\') => Option.empty
      case '\\' => Option.apply(BS_CHARS)
      case c if (c > '"') => Option.empty
      case '"' => Option.apply(QUOT_CHARS)
      case c if (ch > CONTROL_CHARACTERS_END) => Option.empty
      case '\n' => Option.apply(LF_CHARS)
      case '\r' => Option.apply(CR_CHARS)
      case '\t' => Option.apply(TAB_CHARS)
      case _ => Option.apply(Array[Char]('\\', 'u', '0', '0', HEX_DIGITS(ch >> 4 & 0x000f), HEX_DIGITS(ch & 0x000f)))
    }
  }

}

class JsonWriter(writer: Writer) {

  import JsonWriter._

/**
  def this(writer: Writer) = {
    this(writer)
  }
**/
  // all methods throw IOException

  def writeLiteral(value:String): Unit = {
      writer.write(value)
  }

  def writeNumber(value:String): Unit = {
    writer.write(value)
  }

  def writeString(value:String): Unit = {
    writer.write("\"")
    writeJsonString(value)
    writer.write("\"")
  }

  def writeArrayOpen(): Unit = {
    writer.write("[")
  }

  def writeArrayClose(): Unit = {
    writer.write("]")
  }

  def writeArraySeperator(): Unit = {
    writer.write(",")
  }

  def writeObjectOpen(): Unit = {
    writer.write("{")
  }

  def writeObjectClose(): Unit = {
    writer.write("}")
  }

  def writeMemberName(name:String): Unit = {
    writer.write("\"")
    writeJsonString(name)
    writer.write("\"")
  }

  def writeMemberSeperator(): Unit = {
    writer.write(":")
  }

  def writeObjectSeperator(): Unit = {
    writer.write(",")
  }

  def writeJsonString(string:String): Unit = {
    val length:Int = string.length
    val start:Int = 0

    for (index <- 0 to length-1) {
      // replacement can be an option
      // handle all null pointers with options/either
      val replacement = getReplacementChars(string.charAt(index))

      if (replacement.isDefined) {
        replacement.map(item => {
          writer.write(string, start, index-start)
          writer.write(item.toArray)
        })
      }

    }

    writer.write(string, start, length-start)
  }

}
