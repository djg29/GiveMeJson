package dhr.json.parser

class JsonString(string:String) extends JsonValue {

  override def write(writer:JsonWriter): Unit = {
    writer.writeString(string)
  }

  override def isString(): Boolean = true

  override def asString(): String = string

  override def hashCode(): Int = {
    string.hashCode
  }

  override def equals(obj:scala.Any): Boolean = {
    obj match {
      case c if (this == c) => true
      case null => false
      case c if (c.getClass != getClass) => false
      case _ => {
        string.equals(obj.asInstanceOf[JsonString].toString());
      }
    }
  }

}
