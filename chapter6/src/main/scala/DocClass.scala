/**
 * Created by aihe on 11/20/15.
 */
object DocClass {

  type Feature = Set[String]

  def getWords(doc: String): Feature = {
    (for {
      word <- doc.split("\\W+")
      if word.length > 2 && word.length < 20
    } yield word.toLowerCase).toSet

  }

  trait Classifier {

    def featuresFunc: String => Feature

    /**
     *
     * @param cat
     * @param t
     */
    def setThreshold(cat: String, t: Double)

    /**
     *
     * @param cat
     * @return
     */
    def getThreshold(cat: String): Double

    /**
     * Increase the count of a feature/category pair
     * @param f
     * @param cat
     */
    def incf(f: String, cat: String)

    /**
     * Increase the count of a category
     * @param cat
     */
    def incc(cat: String)

    /**
     * The number of times a feature has appeared in a category
     * @param f
     * @param cat
     */
    def fCount(f: String, cat: String): Int

    /**
     * The number of items in a category
     * @param cat
     */
    def catCount(cat: String): Int

    /**
     * The total number of items
     */
    def totalCount(): Int

    /**
     * The list of all categories
     */
    def categories: Feature

    /**
     * Train a classifier
     * @param item
     * @param cat
     */
    def train(item: String, cat: String)

    /**
     * Get probability of a feature occurring at a certain category
     * @param f
     * @param cat
     * @return
     */
    def fProb(f: String, cat: String, alpha: Double = 1.0): Double

    /**
     * Weighted probability for normalization
     * @param f
     * @param cat
     * @param weight
     * @param ap
     * @return
     */
    def weightedProb(f: String, cat: String, weight: Double = 1.0, ap: Double = 0.5): Double
  }

  class ClassifierOnDict(override val featuresFunc: String => Feature) extends Classifier {

    import scala.collection.mutable

    private val fc = mutable.Map.empty[String, mutable.Map[String, Int]]
    private val cc = mutable.Map.empty[String, Int]
    private val thresholds = mutable.Map.empty[String, Double]

    def setThreshold(cat: String, t: Double) = {
      thresholds += cat -> t
    }

    def getThreshold(cat: String): Double = {
      thresholds.getOrElse(cat, 1.0)
    }

    /**
     * Increase the count of a feature/category pair
     * @param f
     * @param cat
     */
    override def incf(f: String, cat: String): Unit = {
      fc.get(f) match {
        case Some(x) => x += cat -> (x.getOrElse(cat, 0) + 1)
        case None => fc += f -> mutable.Map[String, Int](cat -> 1)
      }
    }

    /**
     * Increase the count of a category
     * @param cat
     */
    override def incc(cat: String): Unit = {
      cc += cat -> (cc.getOrElse(cat, 0) + 1)
    }

    /**
     * The total number of items
     */
    override def totalCount(): Int = {
      cc.values.sum
    }

    /**
     * The list of all categories
     */
    override def categories: Feature = cc.keys.toSet

    /**
     * Train a classifier
     * @param item
     * @param cat
     */
    override def train(item: String, cat: String): Unit = {
      featuresFunc(item).foreach(x => {
        incf(x, cat)
      })
      incc(cat)
    }

    /**
     * The number of times a feature has appeared in a category
     * @param f
     * @param cat
     */
    override def fCount(f: String, cat: String): Int = {
      fc.get(f).flatMap(_.get(cat)).getOrElse(0)
    }

    /**
     * The number of items in a category
     * @param cat
     */
    override def catCount(cat: String): Int = {
      cc.getOrElse(cat, 0)
    }

    /**
     * Get probability of a feature occurring at a certain category
     * @param f
     * @param cat
     * @return
     */
    override def fProb(f: String, cat: String, alpha: Double): Double = {
      val cCount = catCount(cat)
      //      if (cCount == 0) 0.0 else fCount(f, cat).toDouble / cCount
      (fCount(f, cat).toDouble + alpha * 1) / (cCount + alpha * totalCount())
    }

    /**
     * Weighted probability for normalization
     * @param f
     * @param cat
     * @param weight
     * @param ap
     * @return
     */
    override def weightedProb(f: String, cat: String, weight: Double, ap: Double): Double = {
      val fC = fCount(f, cat)
      val totals = categories.map(fCount(f, _)).sum
      (fC * totals + weight * ap) / (weight + totals)
    }

  }

  object ClassifierOnDict {
    def apply(featuresFunc: String => Feature): ClassifierOnDict = {
      new ClassifierOnDict(featuresFunc)
    }
  }

  trait NaiveBayes extends Classifier {

    def docProb(item: String, cat: String): Double = {
      val features = featuresFunc(item)
      features.map(fProb(_, cat)).product
    }

    def prob(item: String, cat: String): Double = {
      docProb(item, cat) * catCount(cat) / totalCount()
    }

    def classify(item: String): Option[String] = {
      val m = Map[String, Double](categories.toSeq.map(c => c -> prob(item, c)): _*)
      val (bestCat, bestP) = m.maxBy(_._2)
      (m - bestCat).exists { case (cat: String, p: Double) =>
        p * getThreshold(bestCat) > bestP
      } match {
        case true => None
        case false => Some(bestCat)
      }
    }
  }

  class NaiveBayesOnDict(override val featuresFunc: String => Feature)
    extends ClassifierOnDict(featuresFunc) with NaiveBayes

  object NaiveBayesOnDict {
    def apply(featuresFunc: String => Feature): NaiveBayesOnDict = {
      new NaiveBayesOnDict(featuresFunc)
    }
  }

}
