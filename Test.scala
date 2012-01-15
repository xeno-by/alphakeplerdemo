// build this with "scalac -Xmacros Test.scala"
object Test extends App {
  import Rx._
  val s = "foo = bar"
  s.forAllMatches("""^\s*(?<key>.*?)\s*=\s*(?<value>.*)\s*$""", println("key = %s, value = %s".format(key, value)))
}
