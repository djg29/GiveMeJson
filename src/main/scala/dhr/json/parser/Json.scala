package dhr.json.parser

import java.io.Reader


object Json {

  val NULL = new JsonLiteral("null")
  val TRUE = new JsonLiteral("true")
  val FALSE = new JsonLiteral("false")

  def value(value:Int): JsonValue = {
    new JsonNumber(value.toString)
  }

  def value(value:Long): JsonValue = {
    new JsonNumber(value.toString)
  }

  def value(value:Float): JsonValue = {
    if (value.isInfinite || value.isNaN) {
      throw new IllegalArgumentException("Infinite and NaN values not permitted in JSON")
    }
    new JsonNumber(cutOffPointZero(value.toString))
  }

  def value(value:Double): JsonValue = {
    if (value.isInfinite || value.isNaN) {
      throw new IllegalArgumentException("Infinite and NaN values not permitted in JSON")
    }
    new JsonNumber(cutOffPointZero(value.toString))
  }

  def value(value:Boolean): JsonValue = {
    value match {
      case true => TRUE
      case false => FALSE
    }
  }

  def value(value:String): JsonValue = {
    value match {
      case null => new JsonString("null")
      case _ => new JsonString(value)
    }
  }

  def array(): JsonArray = new JsonArray

  def array(values:Array[Int]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def array(values:Array[Long]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def array(values:Array[Float]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def array(values:Array[Double]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def array(values:Array[Boolean]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def array(values:Array[String]): JsonArray = {
    require(values != null, "values is null")
    var array:JsonArray = new JsonArray
    for (value <- values) {
      array.add(value)
    }
    array
  }

  def jsonObject: JsonObject = new JsonObject

  def parse(string: String): JsonValue = {
    require(string != null, "value cannot be null")
    val handler:DefaultHandler = new DefaultHandler
    new JsonParser(handler.asInstanceOf[JsonHandler[Object, Object]]).parse(string)
    handler.getValue()
  }

  def parse(reader: Reader): JsonValue = {
    require(reader != null, "reader cannot be null")
    val handler:DefaultHandler = new DefaultHandler
    new JsonParser(handler.asInstanceOf[JsonHandler[Object, Object]]).parse(reader)
    handler.getValue()
  }

  def cutOffPointZero(string: String): String = {
    if (string.endsWith(".0")) {
      string.substring(0, string.length() - 2)
    } else {
      string
    }
  }

  class DefaultHandler extends JsonHandler[JsonArray, JsonObject] {
    override var parser: JsonParser = _

    var value:JsonValue = _

    override def startArray():JsonArray = {
      new JsonArray()
    }

    override def startObject():JsonObject = {
      new JsonObject()
    }


    override def endNull(): Unit =  {
      value = NULL;
    }

    override def endBoolean(bool:Boolean): Unit =  {
      bool match {
        case true => value = TRUE
        case false => value = FALSE
      }
    }

    override def endString(string:String): Unit =  {
      value = new JsonString(string)
    }

    override def endNumber(string:String): Unit =  {
      value = new JsonNumber(string)
    }

    override def endArray(array:JsonArray): Unit =  {
      value = array;
    }

    override def endObject(obj:JsonObject): Unit =  {
      value = obj;
    }

    override def endArrayValue(array:JsonArray): Unit =  {
      array.add(value)
    }

    override def endObjectValue(obj:JsonObject, name:String): Unit =  {
      obj.add(name, value)
    }

    def getValue():JsonValue = value

  }

}

class Json private {


}
