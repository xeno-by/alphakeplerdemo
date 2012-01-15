import java.io._
import scala.io._

object Test extends App {
  val csv_path = System.getenv("SCALA_HOME") + "/../../test/files/run/macro_csv.csv"
  val fields = Source.fromFile(csv_path).getLines.next.split(";").map{col => col.trim()}
  
  // blocked by https://issues.scala-lang.org/browse/SI-5229
//  scala.reflect.Code.lift({
//    object Csv {
//      case class macro_csv(`phase name`: String, id: String, description: String)
//  
//      object macro_csv {
//        def parse(path: String) = {
//          val lines = scala.io.Source.fromFile(path).getLines.toList
//          lines map { line => line.split(";", -1).toList match {
//            case phase$whitespace$name :: id :: description :: _ => macro_csv(phase$whitespace$name, id, description)
//            case _ => throw new Exception("format error")
//          }}
//        }
//      }
//    }
//
//    Csv.macro_csv.parse(csv_path) foreach println
//  })
}
