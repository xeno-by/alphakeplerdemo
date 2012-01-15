// build this with "scalac -Xmacros Rx.scala"
import scala.util.matching._
import scala.util.matching.Regex._

object Rx {
  implicit def string2rx(pattern: String): Rx = new Rx(pattern)
  class Rx(pattern0: String) {
    val preproc = """\((\?\<(\w+)\>)?(.*?)\)""".r
    val allGroups = preproc.findAllIn(pattern0).matchData.map{_.group(2)}.toList
    val allNames = allGroups.zipWithIndex.map{ case (g, i) => if (g != null) g else i.toString}.toList
    val namedGroups = allGroups.zipWithIndex.collect{ case (g, i) if g != null => g -> i}.toMap
    val pattern = preproc.replaceAllIn(pattern0, {m => val pattern = m.group(3); "(" + pattern + ")"})

    def rmatch(s: String): Iterator[Match] = {
      val r = new Regex(pattern, allNames: _*)
      (r findAllIn s).matchData
    }
  }

  implicit def string2mx(pattern: String): Mx = new Mx(pattern)
}

class Mx(val s: String) {
  def macro forAllMatches(pattern: String, f: _): Unit = {
    import Rx._
    val Literal(Constant(literal: String)) = pattern
    val groupNames = string2rx(literal).namedGroups.keys.toList
    def mkValDef(name: String, value: Tree) = ValDef(Modifiers(), newTermName(name), TypeTree(), value)
    val locals = groupNames map { name => mkValDef(name, Apply(Select(Ident(newTermName("m")), newTermName("group")), List(Literal(Constant(name))))) }

    val rmatch = Apply(Select(pattern, newTermName("rmatch")), List(Select(_this, newTermName("s"))))
    val foreach = Select(rmatch, newTermName("foreach"))
    val body = Block(locals :+ f, Literal(Constant(())))
    val fn = Function(List(ValDef(Modifiers(), newTermName("m"), TypeTree(), EmptyTree)), body)
    Apply(foreach, List(fn))
  }
}
