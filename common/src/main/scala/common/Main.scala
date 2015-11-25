package common


/**
 * Created by aihe on 11/24/15.
 */
object Main {

  def main(args: Array[String]): Unit = {
    //    val table = Table.loadCSV(path = args(0), tableName = args(1), header = args(2).toBoolean)
    //        val table = Table.loadCSV(path = "chapter9/matchmaker.csv", tableName = "test", header = false)
    val table = Table.loadCSV(path = "/Users/aihe/Documents/workspace/fraud-detection/oct_cr_order2.txt",
      tableName = "fraud", header = true)

    println(table.columns.map(_.columnType).zip(table.colNames).mkString("\t"))
    println(table(1))
  }

}
