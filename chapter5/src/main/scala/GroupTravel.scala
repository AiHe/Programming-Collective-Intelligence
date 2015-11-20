import java.text.SimpleDateFormat
import java.util.{Random, Calendar, Date}

import scala.io.Source

/**
 * Created by aihe on 11/19/15.
 */
object GroupTravel {

  val people = Seq(("Seymour", "BOS"),
    ("Franny", "DAL"),
    ("Zooey", "CAK"),
    ("Walt", "MIA"),
    ("Buddy", "ORD"),
    ("Les", "OMA"))

  val destination = "LGA"

  val rand = new Random()

  val sdf = new SimpleDateFormat("HH:mm")
  val ts = for {
    line <- Source.fromFile("schedule.txt").getLines().toList
  } yield {
      val Array(origin, dest, depart, arrive, price) = line.split(",")
      ((origin, dest), (sdf.parse(depart), sdf.parse(arrive), price.toInt))
    }

  def getMinutes(d: Date): Int = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal.get(Calendar.HOUR_OF_DAY) * 60 + cal.get(Calendar.MINUTE)
  }

  def getTimeInDay(d: Date): String = {
    new SimpleDateFormat("HH:mm").format(d)
  }

  val flights = ts.groupBy(_._1).mapValues(x => x.map(_._2))

  def printSchedule(r: Seq[Int]): Unit = {
    val ft = r.sliding(2, 2).toSeq.map(x => {
      val Seq(f, t) = x
      f -> t
    })
    (people, ft).zipped.toList.foreach { case ((name, origin), (f, t)) => {
      val out = flights((origin, destination))(f)
      val ret = flights((destination, origin))(t)
      println("%10s%10s %5s-%5s $%3s %5s-%5s $%3s".format(name, origin,
        getTimeInDay(out._1), getTimeInDay(out._2), out._3, getTimeInDay(ret._1), getTimeInDay(ret._2), ret._3))
    }
    }
  }

  def scheduleCost(sol: Seq[Int]): Int = {
    val ft = sol.sliding(2, 2).toSeq.map(x => {
      val Seq(f, t) = x
      f -> t
    })
    val tuples = (people, ft).zipped.toList.map { case ((_, origin), (f, t)) => {
      val out = flights((origin, destination))(f)
      val ret = flights((destination, origin))(t)
      (out._3 + ret._3, getMinutes(out._2), getMinutes(ret._1))
    }
    }

    val totalPrice = tuples.map(_._1).sum
    val latestArrival = tuples.map(_._2).max
    val earliestDep = tuples.map(_._3).min

    val totalWait = (people, ft).zipped.toList.map { case ((_, origin), (f, t)) => {
      val out = flights((origin, destination))(f)
      val ret = flights((destination, origin))(t)
      latestArrival - getMinutes(out._2) + getMinutes(ret._1) - earliestDep
    }
    }.sum

    totalPrice + totalWait + (if (latestArrival > earliestDep) 50 else 0)
  }

  private def random(domain: Seq[(Int, Int)]): Seq[Int] = {
    for {
      (min, max) <- domain
    } yield rand.nextInt((max - min) + 1) + min
  }

  def randomOptimize(domain: Seq[(Int, Int)], costFunc: Seq[Int] => Int): Seq[Int] = {
    (1 to 1000).foldLeft((Int.MaxValue, Seq.empty[Int])) {
      case ((m, ret), i) => {
        val n = random(domain)
        val v = costFunc(n)
        if (m < v) (m, ret) else (v, n)
      }
    }
  }._2

  def hillClimb(domain: Seq[(Int, Int)], costFunc: Seq[Int] => Int, maxIter: Int = 1000): Seq[Int] = {
    val init = random(domain)

    def genNeighbors(curSol: Seq[Int]): Seq[Seq[Int]] = {
      (for {
        i <- curSol.indices
      } yield {
          val a = if (curSol(i) + 1 <= domain(i)._2) curSol.take(i) ++ Seq(curSol(i) + 1) ++
            curSol.drop(i + 1)
          else Seq.empty[Int]
          val b = if (curSol(i) - 1 >= domain(i)._1) curSol.take(i) ++ Seq(curSol(i) - 1) ++
            curSol.drop(i + 1)
          else Seq.empty[Int]
          Seq(a, b)
        }).flatten
    }

    def go(curSol: Seq[Int], maxIter: Int = 1000): Seq[Int] = {
      if (maxIter != 0) {
        val neighboes = genNeighbors(curSol)
        val newDir = neighboes.foldLeft((Int.MaxValue, Seq.empty[Int])) {
          case ((m, ret), nei) => {
            if (nei.nonEmpty) {
              val v = costFunc(nei)
              if (m < v) (m, ret) else (v, nei)
            } else {
              (m, ret)
            }
          }
        }._2

        if (curSol == newDir) newDir else go(newDir, maxIter - 1)
      } else {
        curSol
      }
    }
    go(init, maxIter = maxIter)
  }

}
