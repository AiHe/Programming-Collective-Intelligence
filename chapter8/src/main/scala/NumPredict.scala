import scala.math.random

/**
 * Created by aihe on 11/23/15.
 */
object NumPredict {

  type Feature = IndexedSeq[Double]
  type DataSet = IndexedSeq[(Feature, Double)]

  def winPrice(f: Feature): Double = {
    val IndexedSeq(rating, age) = f
    val peakAge = rating - 50
    val price = rating / 2
    if (age > peakAge) {
      price * (5 - (age - peakAge))
    } else {
      val p = price * (5 * (age + 1) / peakAge)
      if (p < 0) 0 else p
    }
  }

  def winSet: DataSet = {
    for (i <- 1 to 300) yield {
      val rating = random * 50 + 50
      val age = random * 50
      val price = winPrice(IndexedSeq(rating, age)) * (random * 0.4 + 0.8)
      IndexedSeq(rating, age) -> price
    }
  }

  def euclidean(v1: Feature, v2: Feature): Double = {
    math.sqrt(v1.zip(v2).map { case (e1, e2) => math.pow(e1 - e2, 2) }.sum)
  }

  def getDistances(rows: DataSet, vec: Feature): IndexedSeq[(Double, Double, Int)] = {
    rows.view.zipWithIndex.map { case ((f, v), i) => (euclidean(f, vec), v, i) }.sortBy(_._1).toIndexedSeq
  }

  class AverageClass(vs: Seq[Double], ws: Seq[Double]) {
    def average: Double = vs.zip(ws).map { case (v, w) => v * w }.sum / ws.sum
  }

  implicit def needAverage(seq: Seq[Double]): AverageClass = new AverageClass(seq, Array.fill[Double](seq.size)(1))

  implicit def needWeightedAverage(seq: Seq[(Double, Double)]): AverageClass = {
    val (vs, ws) = seq.unzip
    new AverageClass(vs, ws)
  }


  class TwoStarOpClass(v: Double) {
    def **(p: Double): Double = math.pow(v, p)
  }

  implicit def needTwoStarOp(v: Double): TwoStarOpClass = new TwoStarOpClass(v)


  def knnEstimate(rows: DataSet, vec: Feature, k: Int = 3): Double = {
    val dists = getDistances(rows, vec)
    require(k > 0)
    dists.take(k).map(_._2).average
  }

  trait InverseFunc {
    def weight(dist: Double, args: Double*): Double
  }

  object InverseWeight extends InverseFunc {
    override def weight(dist: Double, args: Double*): Double = {
      val (num, const) = args match {
        case Seq(n) => (n, 0.1)
        case Seq(n, c) => (n, c)
        case _ => (1.0, 0.1)
      }
      num / (dist + const)
    }
  }

  object SubtractWeight extends InverseFunc {
    override def weight(dist: Double, args: Double*): Double = {
      val const = args match {
        case Seq(c) => c
        case _ => 1.0
      }
      if (dist > const) 0 else const - dist
    }
  }

  object GaussianWeight extends InverseFunc {
    override def weight(dist: Double, args: Double*): Double = {
      val sigma = args match {
        case Seq(s) => s
        case _ => 1.0
      }
      math.E ** (-(dist ** 2) / (sigma ** 2 * 2))
    }
  }

  /**
   * Get distances
   * @param rows
   * @param vec
   * @param k
   * @param weightf
   * @return
   */
  def weightedKnn(rows: DataSet, vec: Feature, k: Int = 5, weightf: InverseFunc = GaussianWeight): Double = {
    val dists = getDistances(rows, vec)
    require(k > 0)
    dists.take(5).map(a => (a._2, GaussianWeight.weight(a._1))).average
  }

}
