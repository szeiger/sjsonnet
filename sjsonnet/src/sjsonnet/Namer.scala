package sjsonnet

import scala.collection.mutable

class Namer(indices: mutable.HashMap[String, Int], names: mutable.ArrayBuffer[String]) {
  def apply(s: String): Namer.Name = indices.getOrElseUpdate(s, {
    val i = names.length
    names.addOne(s)
    i
  })

  def name(n: Namer.Name): String =
    if(n < names.length) names(n)
    else null

  def copy(): Namer = new Namer(mutable.HashMap.from(indices), mutable.ArrayBuffer.from(names))
}

object Namer {
  type Name = Int
  def empty: Namer = {
    val n = new Namer(new mutable.HashMap, new mutable.ArrayBuffer)
    n(null) // register NoName -> null
    n
  }
  val NoName = 0
}
