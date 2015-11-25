/**
 * Created by aihe on 11/24/15.
 */
object AdvancedClassify {

  import common._

  val matchTable = Table.loadCSV(path = "matchmaker.csv", tableName = "MatchMaker", header = false)

}
