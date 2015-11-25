package common

/**
 * Created by aihe on 11/24/15.
 */
case class Row(table: Table, index: Int) {
  require(index >= 0 && index < table.length)

  lazy val values = table.columns.map(_.data(index))

  val names = table.colNames

  lazy val valuesMap = Map(names.zip(values): _*)

  val length = names.length

  val size = length

  override def toString = {
    names.mkString("", "\t", "\n") + values.mkString("\t")
  }

  def apply(index: Int) = {
    values(index)
  }

  val columns = table.columns

}

case object Row {
  def load(data: IndexedSeq[String], tableName: String): Row = {
    Table.load(IndexedSeq(data), tableName, header = false)(0)
  }
}
