package dhr.json.parser

abstract class JsonHandler[A, O] {

  var parser:JsonParser

  def getLocation(): Location = {
    parser.getLocation()
  }

  def startNull(): Unit = {}

  def endNull(): Unit = {}

  def startBoolean(): Unit = {}

  def endBoolean(value:Boolean): Unit = {}

  def startString(): Unit = {}

  def endString(string:String): Unit = {}

  def startNumber(): Unit = {}

  def endNumber(string:String): Unit = {}

  def startArray(): A

  def endArray(array:A): Unit = {}

  def startArrayValue(array:A): Unit = {}

  def endArrayValue(array:A): Unit = {}

  def startObject(): O

  def endObject(obj:O): Unit = {}

  def startObjectName(obj:O): Unit = {}

  def endObjectName(obj:O, name:String): Unit = {}

  def startObjectValue(obj:O, name:String): Unit = {}

  def endObjectValue(obj:O, name:String): Unit = {}


}
