package dhr.json.parser

import java.io.Reader

object JsonArray {

  def readFrom(reader: Reader): JsonArray = {
    JsonValue.readFrom(reader).asArray
  }

  def readFrom(string: String): JsonArray = {
    JsonValue.readFrom(string).asArray
  }

}

class JsonArray(var values: List[JsonValue]) extends JsonValue with Iterable[JsonValue] {

  def this() = {
    this(List[JsonValue]())
  }

  def add(value:Int): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:Long): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:Float): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:Double): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:Boolean): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:String): JsonArray = {
    new JsonArray(Json.value(value) :: values)
  }

  def add(value:JsonValue): JsonArray = {
    new JsonArray(value :: values)
  }

  def set(index:Int, value:Long): JsonArray = {
    new JsonArray(values.updated(index, Json.value(value)))
  }

  def set(index:Int, value:Float): JsonArray = {
    new JsonArray(values.updated(index, Json.value(value)))
  }

  def set(index:Int, value:Double): JsonArray = {
    new JsonArray(values.updated(index, Json.value(value)))
  }

  def set(index:Int, value:Boolean): JsonArray = {
    new JsonArray(values.updated(index, Json.value(value)))
  }

  def set(index:Int, value:String): JsonArray = {
    new JsonArray(values.updated(index, Json.value(value)))
  }

  def set(index:Int, value:JsonValue): JsonArray = {
    new JsonArray(values.updated(index, value))
  }

  def remove(index:Int): JsonArray = {
    new JsonArray(values.diff(List[JsonValue](values(index))))
  }

  override def size: Int = values.length

  override def isEmpty:Boolean = values.isEmpty

  def get(index:Int): JsonValue = values(index)

  //def values: List[JsonValue] = values

  override def write(writer: JsonWriter): Unit = {
    writer.writeArrayOpen
    val it = iterator()
    if (it.hasNext) {
      it.next().write(writer)
      while (it.hasNext) {
        writer.writeArraySeperator()
        it.next.write(writer)
      }
    }
    writer.writeArrayClose
  }

  override def iterator(): Iterator[JsonValue] = {
    val iterate = values.iterator

    val it = new Iterator[JsonValue] {

      def hasNext():Boolean = iterate.hasNext

      override def next(): JsonValue = iterate.next

      def remove = throw new UnsupportedOperationException

    }

    it

  }

  override def isArray: Boolean = true

  override def asArray: JsonArray = this
}
