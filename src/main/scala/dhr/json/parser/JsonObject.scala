package dhr.json.parser

import java.io.{ObjectInputStream, StringWriter}

import dhr.json.parser.JsonObject.{HashIndexTable, Member}

object JsonObject {


  /**
    * Represents a member of a JSON object, a pair of a name and a value.
    */
  class Member(var name:String, var value:JsonValue) {

    /**
      * Returns the name of this member.
      *
      * @return the name of this member, never <code>null</code>
      */
    def getName:String = name

    /**
      * Returns the value of this member.
      *
      * @return the value of this member, never <code>null</code>
      */
    def getValue:JsonValue = value

    override def hashCode(): Int = {
      var result:Int = 1
      result = 31 * result + name.hashCode()
      result = 31 * result + value.hashCode()
      result
    }

    //override def equals(obj: scala.Any): Boolean = super.equals(obj)




  }

  class HashIndexTable(var hashTable:Array[Byte] = new Array[Byte](32)) {

    def this(original: HashIndexTable) {
      this(original.hashTable)
    }

    def add(name: String, index: Int): Unit = {
      val slot: Int = hashSlotFor(name)
      if (index < 0xff) {
        // increment by 1, 0 stands for empty
        hashTable(slot) = (index + 1).toByte
      } else {
        hashTable(slot) = 0
      }
    }

    def remove(index: Int): Unit = {
      for (i <- 0 to hashTable.length) {
        if ((hashTable(i) & 0xff) == index + 1) {
          hashTable(i) = 0
        } else if ((hashTable(i) & 0xff) > index + 1) {
          //hashTable(i).
        }
      }
    }

    def get(name: Object): Int = {
      val slot: Int = hashSlotFor(name)
      // subtract 1, 0 stands for empty
      (hashTable(slot) & 0xff) - 1
    }

    private def hashSlotFor(element: Object): Int = element.hashCode() & hashTable.length - 1

  }

}

class JsonObject(var names:List[String], var values:List[JsonValue], @transient var table: HashIndexTable) extends JsonValue with Iterable[Member] {


  import JsonObject.Member
  /**
    * Creates a new empty JsonObject.
    */
  def this() {
    this(List[String](), List[JsonValue](), new HashIndexTable())
  }

  /**
    * Creates a new JsonObject, initialized with the contents of the specified JSON object.
    *
    *  obj
    *          the JSON object to get the initial contents from, must not be <code>null</code>
    */
  def this(obj:JsonObject) {
    this(obj.names, obj.values, new HashIndexTable())
  }

  def add(name:String, value:Int): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:Long): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:Float): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:Double): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:Boolean): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:String): JsonObject = {
    add(name, Json.value(value))
  }

  def add(name:String, value:JsonValue): JsonObject = {
    require(name != null, "name is  null")
    require(value != null, "value is null")
    table.add(name, names.length)
    names = name :: names
    values = value :: values
    this
  }

  def set(name:String, value:Int): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:Long): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:Float): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:Double): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:Boolean): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:String): JsonObject = {
    set(name, Json.value(value))
  }

  def set(name:String, value:JsonValue): JsonObject = {
    require(name != null, "name is null")
    require(value != null, "value is null")
    val index:Int = indexOf(name)
    if (index != -1) {
      values.updated(index, value)
      //values.patch(index, Seq(value), 1)
    } else {
      table.add(name, names.length)
      names = name :: names
      values = value :: values
    }
    this
  }

  def remove(name:String): JsonObject = {
    val index:Int = indexOf(name)
    if (index != -1) {
      table.remove(index)
      names = names.patch(index, Nil, 1)
      values = values.patch(index, Nil, 1)
    }
    return this
  }

  def contains(name:String): Boolean = {
    names.contains(name)
  }

  def merge(obj:JsonObject): JsonObject = {
    require(obj != null, "object is null")
    //var ob = obj.asInstanceOf[JsonObject.Member]
//    for (member <- obj) {
//      new JsonObject(member.asInstanceOf[Member].getName, member.asInstanceOf[Member].getValue)
//    }
    this
  }

  def get(name:String): JsonValue = {
    require(name != null, "name is null")
    val index:Int = indexOf(name)
    if (index != -1) {
      values(index)
    } else {
      null
    }
  }

  def get(name:String, defaultValue:Int): Int = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asInt
    } else {
      defaultValue
    }
  }

  def get(name:String, defaultValue:Long): Long = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asLong
    } else {
      defaultValue
    }
  }

  def get(name:String, defaultValue:Float): Float = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asFloat
    } else {
      defaultValue
    }
  }

  def get(name:String, defaultValue:Double): Double = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asDouble
    } else {
      defaultValue
    }
  }

  def get(name:String, defaultValue:Boolean): Boolean = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asBoolean
    } else {
      defaultValue
    }
  }

  def get(name:String, defaultValue:String): String = {
    val value:JsonValue = get(name)
    val index:Int = indexOf(name)
    if (value != null) {
      value.asString
    } else {
      defaultValue
    }
  }

  override def size:Int = names.length

  override def isEmpty:Boolean = names.isEmpty

  def iterator():Iterator[Member] = {

    val namesIterator:Iterator[String] = names.iterator
    val valuesIterator:Iterator[JsonValue] = values.iterator

    val it = new Iterator[Member] {

      def hasNext():Boolean = namesIterator.hasNext

      override def next(): Member = {
        new Member(namesIterator.next, valuesIterator.next)
      }

      //def remove() = throw UnsupportedOperationException

    }

    it

  }

  override def write(writer:JsonWriter): Unit = {
    writer.writeObjectOpen
    val namesIterator:Iterator[String] = names.iterator
    val valuesIterator:Iterator[JsonValue] = values.iterator
    if (namesIterator.hasNext) {
      writer.writeMemberName(namesIterator.next())
      writer.writeMemberSeperator()
      valuesIterator.next().write(writer)
      while (namesIterator.hasNext) {
        writer.writeObjectSeperator()
        writer.writeMemberName(namesIterator.next())
        writer.writeMemberSeperator()
        valuesIterator.next().write(writer)
      }
    }
    writer.writeObjectClose()
  }

  override def isObject: Boolean = true

  override def asObject: JsonObject = this

  override def hashCode: Int = {
    var result:Int = 1
    result = 31 * result + names.hashCode()
    result = 31 * result + values.hashCode()
    result
  }

  override def equals(obj:scala.Any): Boolean = {

    obj match {
      case c if (this == c) => true
      case null => false
      case c if (c.getClass != getClass) => false
      case _ => {
        names.equals(obj.asInstanceOf[JsonObject].names) && values.equals(obj.asInstanceOf[JsonObject].values);
      }
    }

  }

  def indexOf(name:String): Int = {
    val index:Int = table.get(name)
    if (index != -1 && name.equals(names(index))) {
      index
    }
    names.lastIndexOf(name)
  }

  def updateHashIndex(): Unit = {
    val size:Int = names.length
    for (i <- 0 to size) {
      table.add(names(i), i)
    }
  }

  def readObject(inputStream: ObjectInputStream): Unit = {
    inputStream.defaultReadObject()
    table = new HashIndexTable()
    updateHashIndex
  }

  override def toString(): String = {
    toString(WriterConfig.MINIMAL)
  }

  override def toString(config: WriterConfig): String = {
    val writer = new StringWriter
    writeTo(writer, config)
    writer.toString
  }

}
