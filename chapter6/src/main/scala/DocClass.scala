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
    def categories(): Feature

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
    def fProb(f: String, cat: String): Double
  }

  class ClassifierOnDict(featuresFunc: String => Feature) extends Classifier {

    import scala.collection.mutable

    private val fc = mutable.Map[String, mutable.Map[String, Int]]()
    private val cc = mutable.Map[String, Int]()

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
    override def categories(): Feature = cc.keys.toSet

    /**
     * Train a classifier
     * @param item
     * @param cat
     */
    override def train(item: String, cat: String): Unit = {
      featuresFunc(item).foreach(x => {
        incf(x, cat)
        incc(cat)
      })
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
    override def fProb(f: String, cat: String): Double = {
      val cCount = catCount(cat)
      if (cCount == 0) 0.0 else fCount(f, cat).toDouble / cCount
    }

    /**
     *
     * @param f
     * @param cat
     * @param weight
     * @param ap
     * @return
     */
    def weightedProb(f: String, cat: String, weight: Double = 1.0, ap: Double = 0.5): Double = {
      val fC = fCount(f, cat)
      val cC = catCount(cat)
      if (fC == 0) ap else (fC + weight * ap) / (weight + cC)
    }
  }

}
