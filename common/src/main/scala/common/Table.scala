package common

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.io.Source
import scala.util.{Failure, Try, Success, Random}


import scala.reflect.runtime.universe._

/**
 * Created by aihe on 11/24/15.
 */

class StringConversion(s: String) {

  val int = """^[-+]?\d+$"""
  val double = """^[-+]?(\d+\.?\d*|\.\d+)([Ee][+-]?\d+)?$"""

  def get: (Any, Type) = {
    //      Try(s.toInt) orElse Try(s.toLong) orElse Try(s.toDouble) orElse Try(s.toBoolean) match {
    //        case Success(i: Int) => (i, typeOf[Int])
    //        case Success(l: Long) => (l, typeOf[Long])
    //        case Success(d: Double) => (d, typeOf[Double])
    //        case Success(b: Boolean) => (b, typeOf[Boolean])
    //        case _ => (s, typeOf[String])
    //      }
    if (s.matches(int)) {
      Try(s.toLong) match {
        //          case Success(l: Long) => if (Int.MinValue <= l && l <= Int.MaxValue) (l.toInt, typeOf[Int]) else (l, typeOf[Long])
        case Success(l: Long) => (l, typeOf[Long])
        case Failure(_) => (s, typeOf[String])
      }
    } else if (s.matches(double)) {
      Try(s.toDouble) match {
        case Success(d: Double) => (d, typeOf[Double])
        case Failure(_) => (s, typeOf[String])
      }
    }
    else (s, typeOf[String])
  }

}


case class Table(tableName: String, columns: Seq[GenericColumn] = Seq.empty) {
  require(columns.map(_.name).distinct.length == columns.length)
  require(columns.isEmpty || columns.map(_.data.size).distinct.size == 1)

  val table: Table = this

  val length = columns.length match {
    case 0 => 0
    case _ => columns.head.data.size
  }

  val size = length

  val width = columns.length

  val colNames = columns.map(_.name)

  lazy val rows = (0 until length).map(Row(this, _))

  lazy val columnsMap = Map(colNames.zip(columns): _*)

  override def toString = {
    colNames.mkString("", "\t", "\n") + rows.map(_.values.mkString("\t")).mkString("\n")
  }

  def apply(index: Int): Row = {
    require(index >= 0 && index < length)
    rows(index)
  }

  def apply(colName: String): GenericColumn = {
    require(colNames.indexOf(colName) > 0)
    columnsMap(colName)
  }

  def nonEmpty: Boolean = length > 0

  def isEmpty: Boolean = !nonEmpty

  def head: Row = rows.head

  def last: Row = rows.last

  def headOption: Option[Row] = rows.headOption

  def partition(p: Row => Boolean): (Table, Table) = {
    val (r1, r2) = rows.partition(p(_))
    Table.fromRows(tableName + "_true", r1) -> Table.fromRows(tableName + "_false", r2)
  }

  def column(col: Int) = {
    require(col >= 0 && col < width)
    columns(col)
  }

  def firstColumn = {
    columns.head
  }

  def lastColumn = {
    columns.last
  }

  def lastColumnOption = {
    columns.lastOption
  }

}

case object Table {

  private val r = new Random()

  implicit def String2ExtendedString(s: String): StringConversion = new StringConversion(s)

  private def colDataCast(colName: String, values: Seq[Any], t: Type): GenericColumn = {
    if (t =:= typeOf[Long]) {
      Column[Long](colName, values.asInstanceOf[Seq[Long]])
    } else if (t =:= typeOf[Double]) {
      Column[Double](colName, values.asInstanceOf[Seq[Double]])
    } else {
      Column[String](colName, values.asInstanceOf[Seq[String]])
    }
  }

  private def feedColumn(colName: String, colData: Seq[String]): GenericColumn = {
    val (vs, typesSample) = r.shuffle[Int, IndexedSeq](colData.indices).toIndexedSeq.take(100).map(colData(_)).map(_.get).unzip
    val uniTypesSample = typesSample.distinct
    if (uniTypesSample.contains(typeOf[String])) {
      Column[String](colName, colData)
    } else {
      val (values, types) = colData.map(_.get).unzip
      val uniTypes = types.distinct
      if (uniTypes.length == 1) {
        colDataCast(colName, values, uniTypes.head)
      } else {
        if (uniTypes.contains(typeOf[Double])) {
          colDataCast(colName, values, typeOf[Double])
        } else {
          colDataCast(colName, values, typeOf[Long])
        }
      }
    }
  }

  def load(data: IndexedSeq[IndexedSeq[String]], tableName: String, header: Boolean = false): Table = {
    val (hs, d) = if (header) {
      val (a, b) = data.splitAt(1)
      (a.head, b)
    } else {
      (data.head.indices.map("col" + _), data)
    }

    val columns = hs.indices.map { case (i: Int) => Future(feedColumn(hs(i), d.map(_(i)))) }

    val fs = Future.sequence(columns).map(Table(tableName, _))

    Await.ready(fs, Duration.Inf).value.get
    match {
      case Success(t: Table) => t
      case _ => throw new Exception
    }
  }

  def loadCSV(path: String, tableName: String, header: Boolean): Table = {
    val data = Source.fromFile(path).getLines().toIndexedSeq.map(_.split(",").toIndexedSeq)
    load(data, tableName, header)
  }

  def loadTSV(path: String, tableName: String, header: Boolean): Table = {
    val data = Source.fromFile(path).getLines().toIndexedSeq.map(_.split("\t").toIndexedSeq)
    load(data, tableName, header)
  }


  def fromRows(tableName: String, rows: IndexedSeq[Row]): Table = {
    if (rows.nonEmpty) {
      val cols = (0 until rows.head.length).map { case i => {
        val colName = rows.head.names(i)
        val t = rows.head.table.column(i).columnType
        val col = if (t =:= typeOf[Long]) {
          Column[Long](colName, rows.map(r => r(i)).asInstanceOf[IndexedSeq[Long]])
        } else if (t =:= typeOf[Double]) {
          Column[Double](colName, rows.map(r => r(i)).asInstanceOf[IndexedSeq[Double]])
        } else {
          Column[String](colName, rows.map(r => r(i)).asInstanceOf[IndexedSeq[String]])
        }
        col.asInstanceOf[GenericColumn]
      }
      }
      Table(tableName, cols)
    } else {
      Table(tableName)
    }
  }
}