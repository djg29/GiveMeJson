package dhr.json.parser

import java.io.{Reader, StringReader}

object JsonParser {

  val MAX_NESTING_LEVEL:Int = 1000
  val MIN_BUFFER_SIZE:Int = 10
  val DEFAULT_BUFFER_SIZE:Int = 1024

}

class JsonParser(var handler: JsonHandler[Object, Object]) {

  import JsonParser._

  //var handler: JsonHandler[Object, Object] = _
  var reader:Reader = _
  var buffer:Array[Char]= new Array[Char](DEFAULT_BUFFER_SIZE)
  var bufferOffset:Int = 0
  var index:Int = 0
  var fill:Int = 0
  var line:Int = 0
  var lineOffset:Int = 0
  var current:Int = 0
  var captureBuffer:StringBuilder = new StringBuilder
  var captureStart:Int = 0
  var nestingLevel:Int = 0

  def parse(reader:Reader, buffersize:Int): Unit = {
    require(reader != null, "reader is null")
    require(buffersize > 0, "beffersize is zero or negative")
    this.reader = reader
    this.buffer = new Array[Char](buffersize)
    bufferOffset = 0
    index = 0
    fill = 0
    line = 1
    lineOffset = 0
    current = 0
    captureStart = -1
    read()
    skipWhiteSpace()
    readValue()
    skipWhiteSpace()
    if (!isEndOfText()) {
      throw error("Unexpected character")
    }
  }

/*
* |                      bufferOffset
*                        v
* [a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t]        < input
*                       [l|m|n|o|p|q|r|s|t|?|?]    < buffer
*                          ^               ^
*                       |  index           fill
*/

  def read(): Unit = {
    if (index == fill) {
      if (captureStart != -1) {
        captureBuffer.append(buffer, captureStart, fill - captureStart)
        captureStart = 0
      }
      bufferOffset += fill
      fill = reader.read(buffer, 0, buffer.length)
      index = 0
      if (fill == -1) {
        current = -1
        index += 1
        return
      }
    }
    if (current == '\n') {
      line += 1
      lineOffset = bufferOffset + index
    }
    current = buffer(index)
    index += 1
  }

  def readValue():Unit = {
    current match {
      case 'n' => readNull()
      case 't' => readTrue()
      case 'f' =>readFalse()
      case '"' => readString()
      case '[' => readArray()
      case '{' => readObject()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => readNumber()
      case _ => {
        println("error")
        throw expected("value")
      }
    }
  }

  def readArray(): Unit = {
    println("read array")
    println("1")
    val array:Object = handler.startArray()
    read()
    println("2")
    nestingLevel += 1
    if (nestingLevel > MAX_NESTING_LEVEL) {
      throw error("Nesting too deep")
    }
    println("3")
    skipWhiteSpace()
    if (readChar(']')) {
      println("4")
      nestingLevel -= 1
      handler.endArray(array)
      return
    }
    do {
      println("5")
      skipWhiteSpace()
      handler.startArrayValue(array)
      readValue()
      println("6")
      handler.endArrayValue(array)
      skipWhiteSpace()
      println("7")
    } while (readChar(','))
    if (!readChar(']')) {
      println("8")
      throw expected("',' or ']'")
    }
    println("9"+ array.toString)
    nestingLevel -= 1
    handler.endArray(array)
  }

  def parse(reader:Reader): Unit = {
    parse(reader, DEFAULT_BUFFER_SIZE)
  }

  def parse(string:String): Unit = {
    require(string !=null, "'string is null")
    val bufferSize:Int = Math.max(MIN_BUFFER_SIZE, Math.min(DEFAULT_BUFFER_SIZE, string.length()))
    parse(new StringReader(string), bufferSize)
  }

  def readObject(): Unit = {
    val obj:Object = handler.startObject()
    read()
    nestingLevel += 1
    if (nestingLevel > MAX_NESTING_LEVEL) {
      throw error("Nesting too deep")
    }
    skipWhiteSpace()
    if (readChar('}')) {
      nestingLevel -= 1
      handler.endObject(obj)
      return
    }
    do {
      skipWhiteSpace()
      handler.startObjectName(obj)
      val name:String = readName()
      handler.endObjectName(obj, name)
      skipWhiteSpace()
      if (!readChar(':')) {
        throw expected("':'")
      }
      skipWhiteSpace()
      handler.startObjectValue(obj, name)
      readValue()
      handler.endObjectValue(obj, name)
      skipWhiteSpace()
    } while (readChar(','))
    if (!readChar('}')) {
      throw expected("',' or '}'")
    }
    nestingLevel -= 1
    handler.endObject(obj)
  }

  def readName():String = {
    //require(current.equals('"'), throw expected("name"))
    readStringInternal()
  }

  def readNull(): Unit = {
    handler.startNull()
    read()
    readRequiredChar('u')
    readRequiredChar('l')
    readRequiredChar('l')
    handler.endNull()
  }

  def readTrue(): Unit = {
    handler.startBoolean()
    read()
    readRequiredChar('r')
    readRequiredChar('u')
    readRequiredChar('e')
    handler.endBoolean(true)
  }

  def readFalse(): Unit = {
    handler.startBoolean()
    read()
    readRequiredChar('a')
    readRequiredChar('l')
    readRequiredChar('s')
    readRequiredChar('e')
    handler.endBoolean(false)
  }

  def readRequiredChar(ch: Char): Unit = {
    if (!readChar(ch)) {
      throw expected("'" + ch + "'")
    }
  }

  def readString(): Unit = {
    handler.startString()
    handler.endString(readStringInternal())
  }

  def readStringInternal(): String = {
    read()
    startCapture()
    while (current != '"') {
      if (current == '\\') {
        pauseCapture()
        readEscape()
        startCapture()
      } else if (current < 0x20) {
        throw expected("valid string character")
      } else {
        read()
      }
    }
    val string:String = endCapture()
    read()
    string
  }

  def readEscape(): Unit = {
    read()

    current match {
      case '"' | '/' | '\\' => captureBuffer.append(current.toChar)
      case 'b' => captureBuffer.append('\b')
      case 'f' => captureBuffer.append('\f')
      case 'n' => captureBuffer.append('\n')
      case 'r' => captureBuffer.append('\r')
      case 't' => captureBuffer.append('\t')
      case 'u' => {
        var hexChars: Array[Char] = new Array[Char](4)
        for (i <- 0 to 4) {
          read()
          if (!isHexDigit()) {
            throw expected("hexadecimal digit")
          }
          hexChars(i) = current.toChar
        }
        captureBuffer.append(Integer.parseInt(new String(hexChars), 16).toChar)
      }
      case _ => throw expected("valid escape sequence")
    }
    read()
  }

  def readNumber(): Unit = {
    println("read number-" + current.toString)
    handler.startNumber()
    startCapture()
    readChar('-')
    val firstDigit:Int = current
    if (!readDigit()) {
      throw expected("digit")
    }
    if (firstDigit != '0') {
      while (readDigit()) {
      }
    }
    readFraction()
    readExponent()
    handler.endNumber(endCapture())
  }

  def readFraction(): Boolean = {
    if (!readChar('.')) {
      false
    }
    else if (!readDigit()) {
      throw expected("digit")
    } else {
      while (readDigit()) {
      }
      true
    }
  }

  def readExponent(): Boolean = {
    if (!readChar('e') && !readChar('E')) {
      false
    }
    else if (!readChar('+')) {
      readChar('-')
    }
    else if (!readDigit()) {
      throw expected("digit")
    }
    else {
      while (readDigit()) {
      }
      true
    }
  }

  def readChar(ch: Char): Boolean = {
    if (current != ch) {
      false
    } else {
      read()
      true
    }
  }

  def readDigit(): Boolean = {
    if (!isDigit()) {
      false
    } else {
      read()
      true
    }
  }

  def skipWhiteSpace(): Unit = {
    while (isWhiteSpace()) {
      read()
    }
  }

  def startCapture(): Unit = {
    if (captureBuffer == null) {
      captureBuffer = new StringBuilder()
    }
    captureStart = index - 1
  }

  def pauseCapture():Unit = {
    val end:Int = if (current == -1) {
      index
    } else {
      index - 1
    }
    captureBuffer.append(buffer, captureStart, end - captureStart)
    captureStart = -1
  }

  def endCapture(): String = {
    val start:Int = captureStart
    val end:Int = index - 1
    captureStart = -1
    if (captureBuffer.length() > 0) {
      captureBuffer.append(buffer, start, end - start)
      var captured:String = captureBuffer.toString()
      captureBuffer.setLength(0)
      captured
    }
    new String(buffer, start, end - start)
  }

  def getLocation(): Location =  {
    var offset:Int = bufferOffset + index - 1
    var column:Int = offset - lineOffset + 1
    new Location(offset, line, column)
  }

  def expected(expected: String): ParseException = {
    if (isEndOfText()) {
      error("Unexpected end of input")
    } else {
      error("Expected " + expected)
    }
  }

  def error(message:String): ParseException = {
    new ParseException(message, getLocation())
  }

  def isWhiteSpace():Boolean = {
    current == ' ' || current == '\t' || current == '\n' || current == '\r'
  }

  def isDigit():Boolean = {
    current >= '0' && current <= '9'
  }

  def isHexDigit():Boolean = {
      current >= '0' && current <= '9' || current >= 'a' && current <= 'f' || current >= 'A' && current <= 'F'
  }

  def isEndOfText():Boolean = {
    current == -1
  }

}
