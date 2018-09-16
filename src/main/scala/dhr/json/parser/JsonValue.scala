package dhr.json.parser

import java.io.{Reader, StringWriter, Writer}

object JsonValue {

  val TRUE: JsonValue = new JsonLiteral("true")
  val FALSE: JsonValue = new JsonLiteral("false")
  val NULL: JsonValue = new JsonLiteral("null")

  def readFrom(reader: Reader): JsonValue = {
    Json.parse(reader)
  }

  def readFrom(text: String): JsonValue = {
    Json.parse(text)
  }

  def valueOf(value: Int): JsonValue = {
    Json.value(value)
  }

  def valueOf(value: Long): JsonValue = {
    Json.value(value)
  }

  def valueOf(value: Float): JsonValue = {
    Json.value(value)
  }

  def valueOf(value: Double): JsonValue = {
    Json.value(value)
  }

  def valueOf(string: String): JsonValue = {
    Json.value(string)
  }

  def valueOf(value: Boolean): JsonValue = {
    Json.value(value)
  }


}

abstract class JsonValue extends Serializable {

  def write(writer: JsonWriter): Unit

  def isObject: Boolean = false

  def isArray: Boolean = false

  def isNumber: Boolean = false

  def isString: Boolean = false

  def isBoolean: Boolean = false

  def isTrue: Boolean = false

  def isFalse: Boolean = false

  def isNull: Boolean = false

  def asObject: JsonObject = throw new UnsupportedOperationException("Not an object:" + toString)

  def asArray: JsonArray = throw new UnsupportedOperationException("Not an array:" + toString)

  def asInt: Int = throw new UnsupportedOperationException("Not an int:" + toString)

  def asLong: Long = throw new UnsupportedOperationException("Not a long:" + toString)

  def asFloat: Float = throw new UnsupportedOperationException("Not a float:" + toString)

  def asDouble: Double = throw new UnsupportedOperationException("Not an double:" + toString)

  def asString: String = throw new UnsupportedOperationException("Not an string:" + toString)

  def asBoolean: Boolean = throw new UnsupportedOperationException("Not an boolean:" + toString)

  def writeTo(writer: Writer): Unit = {
    writeTo(writer, WriterConfig.MINIMAL)
  }

  def writeTo(writer: Writer, config: WriterConfig): Unit = {
    require(writer != null, "writer must be non null")
    require(config != null, "config must be non null")
    val buffer = new WritingBuffer(writer, 128)
    write(config.createWriter(buffer))
    buffer.flush
  }

  override def toString(): String = {
    toString(WriterConfig.MINIMAL)
  }

  def toString(config: WriterConfig): String = {
    val writer = new StringWriter
    writeTo(writer, config)
    writer.toString
  }

}
