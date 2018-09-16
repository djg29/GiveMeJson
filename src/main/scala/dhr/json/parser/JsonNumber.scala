package dhr.json.parser

class JsonNumber(val string:String) extends JsonValue {

  override def toString(): String = {
    string
  }

  override def write(writer:JsonWriter): Unit = {
    writer.writeNumber(string)
  }

  override def isNumber(): Boolean = true

  override def asInt(): Int = string.toInt

  override def asLong(): Long = string.toLong

  override def asFloat(): Float = string.toFloat

  override def asDouble(): Double = string.toDouble

  override def hashCode(): Int = {
    string.hashCode
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case c if (this == c) => true
      case null => false
      case c if (c.getClass != getClass) => false
      case _ => string.equals(obj.asInstanceOf[JsonNumber].string)
    }
  }

}
