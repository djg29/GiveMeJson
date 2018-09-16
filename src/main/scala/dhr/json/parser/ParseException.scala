package dhr.json.parser

class ParseException(string:String, location:Location) extends RuntimeException(string:String) {

  def getLocation:Location = location

  def getOffset:Int = location.offset

  def getLine:Int = location.line

  def getColumn:Int = location.column

}