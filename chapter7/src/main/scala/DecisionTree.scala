import scala.io.Source

/**
 * Created by aihe on 11/22/15.
 */
object DecisionTree {

  val myData = Source.fromFile("decision_tree_example.txt").getLines().toSeq.map(_.split("\t").toSeq)

  sealed trait Node

  class Branch[T](col: String, value: T => Boolean, tb: Node, fb: Node)

  class Leaf(result: String)

  class StringConversion(s: String) {
    val r = """^[-+]?(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?$"""
    val bool = """[true|false]"""

    def convert: Any = {
      if (s.matches(r)) s.toDouble
      else if (s.toLowerCase.matches(bool)) s.toBoolean
      else s
    }
  }

  implicit def String2ExtendedString(s: String): StringConversion = new StringConversion(s)

  def typeCheck(v: Any): Any = {
    v match {
      case z: Boolean => z
      case b: Byte => b.toDouble
      case c: Char => c.toDouble
      case s: Short => s.toDouble
      case i: Int => i.toDouble
      case j: Long => j.toDouble
      case f: Float => f.toDouble
      case d: Double => d.toDouble
      case l: String => l
      case _ => throw new Exception
    }
  }


  /**
   * Divides a set on a specific column Can handle numeric or nominal values
   * @param rows
   * @param column
   * @param value
   */
  def divideSet[T](rows: Seq[Seq[String]], column: Int, value: T): (Seq[Seq[String]], Seq[Seq[String]]) = {
    require(rows.nonEmpty && column >= 0 && column < rows.head.length - 1)
    rows.partition { case seq => {
      val e = seq(column)
      (e.convert, typeCheck(value)) match {
        case (a: Boolean, b: Boolean) => a == b
        case (a: Double, b: Double) => a >= b
        case (a: String, b: String) => a == b
        case _ => throw new Exception
      }

    }
    }
  }

}
