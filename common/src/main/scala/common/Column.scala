package common

import scala.reflect.runtime.universe._

/**
 * Created by aihe on 11/24/15.
 */
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
  override lazy val data: IndexedSeq[Any] = colData.toIndexedSeq

}
