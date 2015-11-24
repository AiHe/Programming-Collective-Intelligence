package common

import scala.io.Source
import scala.reflect.runtime.universe._
import scala.util.{Success, Try}

/**
 * Created by aihe on 11/24/15.
 */
object DataFrame {

  class StringConversion(s: String) {
    //    val number = """^[-+]?(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?$"""
    //    val bool = Set("true", "false", "yes", "no")

    def getType: (Any, Type) = {
      Try(s.toInt) orElse Try(s.toLong) orElse Try(s.toDouble) orElse Try(s.toBoolean) match {
        case Success(i: Int) => (i, typeOf[Int])
        case Success(l: Long) => (l, typeOf[Long])
        case Success(d: Double) => (d, typeOf[Double])
        case Success(b: Boolean) => (b, typeOf[Boolean])
        case _ => (s, typeOf[String])
      }
    }

    //    def convert: Any = {
    //      if (s.matches(number)) s.toDouble
    //      else if (bool.contains(s.toLowerCase)) {
    //        val sl = s.toLowerCase
    //        if (sl == "true" || sl == "yes") true
    //        else false
    //      }
    //      else s
    //    }
  }

  implicit def String2ExtendedString(s: String): StringConversion = new StringConversion(s)

  trait GenericColumn {
    val name: String
    val data: IndexedSeq[Any]
    val columnType: Type
  }

  case class Column[T: TypeTag](colName: String, colData: Seq[T]) extends GenericColumn {
    override def toString = {
      colName + "\n" + colData.mkString("\n")
    }

    override val columnType = typeOf[T]
    override val name: String = colName
    override val data: IndexedSeq[Any] = colData.toIndexedSeq

  }

  case class Row(table: Table, index: Int) {
    require(index >= 0 && index < table.length)

    def values = table.columns.map(_.data(index))

    private val names = table.colNames

    def valuesMap = Map(names.zip(values): _*)

    override def toString = {
      names.mkString("", "\t", "\n") + values.mkString("\t")
    }
  }

  case class Table(tableName: String, columns: Seq[GenericColumn] = Seq.empty) {
    require(columns.map(_.name).distinct.length == columns.length)
    require(columns.map(_.data.size).distinct.size == 1)

    val table: Table = this

    val length = columns.length match {
      case 0 => 0
      case _ => columns.head.data.size
    }

    val width = columns.length

    val colNames = columns.map(_.name)

    lazy val rows = (0 until length).map(Row(this, _))

    lazy val columnsMap = Map(colNames.zip(columns): _*)

    override def toString = {
      colNames.mkString("", "\t", "\n") + rows.map(_.values.mkString("\t")).mkString("\n")
    }

    def apply(index: Int): Row = {
      require(index >= 0 && index < width)
      rows(index)
    }

    def apply(colName: String): GenericColumn = {
      require(colNames.indexOf(colName) > 0)
      columnsMap(colName)
    }

  }

  case object Table {

    private def colDataCast(colName: String, values: Seq[Any], t: Type): GenericColumn = {
      if (t =:= typeOf[Int]) {
        Column[Int](colName, values.asInstanceOf[Seq[Int]])
      } else if (t =:= typeOf[Long]) {
        Column[Long](colName, values.asInstanceOf[Seq[Long]])
      } else if (t =:= typeOf[Double]) {
        Column[Double](colName, values.asInstanceOf[Seq[Double]])
      } else if (t =:= typeOf[Boolean]) {
        Column[Boolean](colName, values.asInstanceOf[Seq[Boolean]])
      } else {
        Column[String](colName, values.asInstanceOf[Seq[String]])
      }
    }

    private def feedColumn(colName: String, colData: Seq[String]): GenericColumn = {
      val (values, types) = colData.map(_.getType).toIndexedSeq.unzip
      val uniTypes = types.distinct
      if (uniTypes.contains(typeOf[String])) {
        colDataCast(colName, values, typeOf[String])
      } else if (uniTypes.length == 1) {
        colDataCast(colName, values, uniTypes.head)
      } else {
        if (!uniTypes.contains(typeOf[Boolean])) {
          if (uniTypes.contains(typeOf[Double])) {
            colDataCast(colName, values, typeOf[Double])
          } else if (uniTypes.contains(typeOf[Long])) {
            colDataCast(colName, values, typeOf[Long])
          } else {
            colDataCast(colName, values, typeOf[Int])
          }
        } else {
          colDataCast(colName, values, typeOf[String])
        }
      }
    }

    def loadCSV(path: String, tableName: String, header: Boolean): Table = {
      val data = Source.fromFile(path).getLines().toIndexedSeq.map(_.split(",").toIndexedSeq)

      val (hs, d) = if (header) {
        val (a, b) = data.splitAt(1)
        (a.head, b)
      } else {
        (data.head.indices.map("col" + _), data)
      }

      val cs = for {
        c <- hs.indices
      } yield d.map(_(c))

      val columns = hs.zip(cs).map { case (h, c) => feedColumn(h, c) }

      Table(tableName, columns)
    }
  }

  def main(args: Array[String]): Unit = {
    val table = Table.loadCSV(path = "chapter9/matchmaker.csv", tableName = "matchmaker", header = false)
    //    val table = Table.loadCSV(path = "test.csv", tableName = "test", header = true)
    println(table.columns.map(_.columnType).mkString("\t"))
    //    println(table)
    println(table("col1"))
  }

}
