import scala.io.Source
import scala.math.log

/**
 * Created by aihe on 11/22/15.
 */
object DecisionTree {

  type Feature = IndexedSeq[String]
  type DataSet = IndexedSeq[Feature]
  type Result = Map[String, Int]

  val myData: DataSet = Source.fromFile("decision_tree_example.txt").getLines().
    toIndexedSeq.map(_.split("\t").toIndexedSeq)

  sealed trait Node

  case class Branch[T](col: Int, value: T, tb: Node, fb: Node) extends Node

  case class Leaf(result: Result) extends Node

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
  def divideSet(rows: DataSet, column: Int, value: Any): (DataSet, DataSet) = {
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

  def uniqueCounts(rows: DataSet): Result = {
    rows.map(_.last).groupBy(x => x).mapValues(_.size)
  }

  trait Impurity {
    def calculate(rows: DataSet): Double
  }

  object Gini extends Impurity {
    /**
     * Probability that a randomly placed item will be in the wrong category
     * @param rows
     */
    def calculate(rows: DataSet): Double = {
      val countMap = uniqueCounts(rows)
      val totalLen = rows.length.toDouble
      (for {
        (k, v) <- countMap
        (k1, v1) <- countMap
        if k != k1
      } yield (v / totalLen) * (v1 / totalLen)).sum
    }
  }


  object Entropy extends Impurity {
    /**
     * Entropy is the sum of p(x)log(p(x)) across all the different possible results
     * @param rows
     */
    def calculate(rows: DataSet): Double = {
      val countMap = uniqueCounts(rows)
      val totalLen = rows.length.toDouble
      countMap.map { case (k, v) =>
        val p = v / totalLen
        -p * log(p) / log(2)
      }.sum
    }
  }

  def buildTree(rows: DataSet, impurity: Impurity = Entropy): Node = {
    require(rows.nonEmpty)
    val numCol = rows.head.length - 1
    val curScore = impurity.calculate(rows)

    val bestGain = Double.MinValue
    val bestCriteria: (Int, Any) = (-1, null)
    val bestSets: (DataSet, DataSet) = (rows, rows)

    val bestAcc = (bestGain, bestCriteria, bestSets)

    val (finalScore, (finalCol, finalValue), (finalSet1, finalSet2)) = (0 until numCol).
      foldLeft(bestAcc) {
      case (acc, c) => {
        rows.map(_(c).convert).foldLeft(acc) {
          case ((bg, bc, bs), v) => {
            val (set1, set2) = divideSet(rows, c, v)
            val portion = set1.size.toDouble / rows.size
            val score = portion * impurity.calculate(set1) + (1 - portion) * impurity.calculate(set2)
            val gain = curScore - score
            if (gain > bg) (gain, (c, v), (set1, set2)) else (bg, bc, bs)
          }
        }
      }
    }

    if (finalScore > 0) {
      val (br1, br2) = (buildTree(finalSet1), buildTree(finalSet2))
      Branch(finalCol, finalValue, br1, br2)
    } else Leaf(uniqueCounts(rows))
  }


  def classify(observation: Feature, tree: Node): Result = {
    tree match {
      case Leaf(result) => result
      case Branch(col, value, tb, fb) => {
        require(col >= 0 && col < observation.size)
        val br = (observation(col).convert, value) match {
          case (a: Boolean, b: Boolean) => if (a == b) tb else fb
          case (a: Double, b: Double) => if (a >= b) tb else fb
          case (a: String, b: String) => if (a == b) tb else fb
          case _ => throw new Exception
        }
        classify(observation, br)
      }
    }
  }

}
