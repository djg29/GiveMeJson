package dhr.json.parser

class Location(val offset:Int, val line:Int, val column:Int) {

  def this() {
    this(0, 0, 0)
  }

}
