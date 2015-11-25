import scala.math.log

/**
 * Created by aihe on 11/22/15.
 */
object DecisionTree {

  import common._

  type Feature = Row
  type DataSet = Table
  type Result = Map[String, Int]


  val myData = Table.loadTSV(path = "chapter7/decision_tree_example.txt", tableName = "dt",
    header = false)

  sealed trait Node

  case class Branch[T](col: Int, value: T, tb: Node, fb: Node) extends Node

  case class Leaf(result: Result) extends Node

  //  class StringConversion(s: String) {
  //    val r = """^[-+]?(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?$"""
  //    val bool = """[true|false]"""
  //
  //    def convert: Any = {
  //      if (s.matches(r)) s.toDouble
  //      else if (s.toLowerCase.matches(bool)) s.toBoolean
  //      else s
  //    }
  //  }
  //
  //  implicit def String2ExtendedString(s: String): StringConversion = new StringConversion(s)

  def typeCheck(v: Any): Any = {
    v match {
      case b: Byte => b.toLong
      case c: Char => c.toLong
      case s: Short => s.toLong
      case i: Int => i.toLong
      case j: Long => j.toLong
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
   * @return
   */
  def divideSet(rows: DataSet, column: Int, value: Any): (DataSet, DataSet) = {
    require(rows.nonEmpty && column >= 0 && column < rows.width)
    divideSet(rows, rows.colNames(column), value)
  }

  /**
   * Divides a set on a specific column Can handle numeric or nominal values
   * @param rows
   * @param column
   * @param value
   */
  def divideSet(rows: DataSet, column: String, value: Any): (DataSet, DataSet) = {
    //    require(rows.nonEmpty && column >= 0 && column < rows.head.length - 1)
    //    rows.partition { case seq => {
    //      val e = seq(column)
    //      (e.convert, typeCheck(value)) match {
    //        case (a: Boolean, b: Boolean) => a == b
    //        case (a: Double, b: Double) => a >= b
    //        case (a: String, b: String) => a == b
    //        case _ => throw new Exception
    //      }
    //    }
    rows.partition { case row => {
      val v1 = row.valuesMap(column)
      val v2 = typeCheck(value)
      (v1, v2) match {
        case (a: Long, b: Long) => a >= b
        case (a: Double, b: Long) => a >= b
        case (a: Long, b: Double) => a >= b
        case (a: Double, b: Double) => a >= b
        case (a: String, b: String) => a == b
        case _ => throw new Exception
      }
    }
    }
  }

  def uniqueCounts(rows: DataSet): Result = {
    //    val t = rows.lastColumn.columnType
    //    if (t =:= typeOf[Long]) {
    //      rows.lastColumn.data.groupBy[Long](x => x).mapValues(_.size)
    //    } else if (t =:= typeOf[Double]) {
    //      rows.lastColumn.data.groupBy[Double](x => x).mapValues(_.size)
    //    } else {
    //      rows.lastColumn.data.groupBy[String](x => x).mapValues(_.size)
    //    }
    rows.lastColumnOption.map(_.data.groupBy(x => x.toString).mapValues(_.size)).getOrElse(Map.empty)
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
        val x = rows.column(c).data
        x.foldLeft(acc) {
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
        val br = (observation.values(col), value) match {
          case (a: Boolean, b: Boolean) => if (a == b) tb else fb
          case (a: Double, b: Double) => if (a >= b) tb else fb
          case (a: String, b: String) => if (a == b) tb else fb
          case _ => throw new Exception
        }
        classify(observation, br)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val data = DecisionTree.myData
    println(data)
    val (a, b) = DecisionTree.divideSet(data, 3, 22)
    println()
    println(a)
    println()
    println(b)
    val tree = DecisionTree.buildTree(DecisionTree.myData)
    println()
    println(tree)

    println(DecisionTree.classify(Row.load(IndexedSeq("(direct)", "USA", "yes", "5"), "test"), tree))
  }

}
