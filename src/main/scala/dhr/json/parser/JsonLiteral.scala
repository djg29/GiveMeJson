package dhr.json.parser

class JsonLiteral(value: String, isNull: Boolean, isTrue:Boolean, isFalse:Boolean) extends JsonValue {

  def this(value: String) = {
    this(value, value.equals("null"), value.equals("true"), value.equals("false"))
  }

  override def write(writer: JsonWriter): Unit = {
    writer.writeLiteral(value)
  }

  // implement getters and setters and equals and hashcode

}
