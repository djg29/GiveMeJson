package dhr.json.parser

import dhr.json.parser

object Main extends App {

  // @TODO: dependency injection
  // @TODO: thread safety - preferably actor model
  // @TODO: features annotations/metadata
  // @TODO: spring integration

  val obj:JsonObject = Json.jsonObject
    //.add("foo", "goo")
    //                                  .add("yo",23)
    //                                  .add("jio", Json.jsonObject.add("foo", "goo").add("loo","doo"))
                                      .add("yum", Json.array.add(Json.jsonObject.add("lub", "dub"))
   //                                     .add(Json.jsonObject.add("gee", "duh"))
                                      //  .add("nei")
                                      //  .add("lei")
                                      )
  val string:String = obj.toString

  println(string)

//  val obj1:JsonArray = Json.array.add("foo")
  //println(string)

//  val json:String = "{\"order\": 4711}"

  def `this is me yo!!`:String = {
    "hahah"
  }

  println(`this is me yo!!`)

  var order: JsonArray = Json.parse(string).asObject.get("yum").asArray

  println(order.toString())

  if (order.isString) {
    println(order.asString)
  }
  else if (order.isArray) {
    println(order.asArray)
  }
}
