package sk.scalagine.math

import scala.annotation.tailrec


/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 18.5.2014
 * Time: 14:22
 */
class InverseOperation[Dimension <: Vector[Dimension]](private val matrix: MatrixNxN[Dimension]) {

  private type MatrixTuple = (List[List[Float]], List[List[Float]])

  def perform(createNewInstance: List[List[Float]] => MatrixNxN[Dimension]): MatrixNxN[Dimension] = {
    @tailrec def perform(matrices: MatrixTuple, column: Int): MatrixTuple = column match {
      case index if index == matrices._1.length => matrices
      case other => perform(processColumn(matrices, column), column + 1)
    }

    createNewInstance(perform((matrix.data, identity(matrix.data.length)), 0)._2)
  }

  private def processColumn(matrices: MatrixTuple, column: Int): MatrixTuple = {
    val updatedMatrices = moveRowWithMaxAbsToTop(matrices, column)
    processRows(multiplyRow(updatedMatrices, column, 1.0f / updatedMatrices._1(column)(column)), column)
  }

  private def processRows(matrices: MatrixTuple, column: Int): MatrixTuple = {
    @tailrec def processRow(matrices: MatrixTuple, row: Int): MatrixTuple = row match {
      case index if index == matrices._1.length => matrices
      case index if index == column => processRow(matrices, row + 1)
      case other => processRow(addRowToRow(matrices, column, row, -matrices._1(row)(column)), row + 1)
    }

    processRow(matrices, 0)
  }

  private def moveRowWithMaxAbsToTop(matrices: MatrixTuple, column: Int): MatrixTuple = {
    val maxAbs: (Float, Int) = matrices._1.transpose.apply(column).zipWithIndex.drop(column).maxBy(_._1)
    val maxAbsRowIndex: Int = maxAbs._2

    if (maxAbs._1 == 0) throw new InverseMatrixNotExistsException(matrix)
    if (maxAbsRowIndex == column) matrices else swap(matrices, (column, maxAbsRowIndex))
  }

  private def swap(matrices: MatrixTuple, indices: (Int, Int)): MatrixTuple = {
    def swap(list: List[List[Float]]) = List.range(0, list.length)
      .map(row => list(row match {
      case index if index == indices._1 => indices._2
      case index if index == indices._2 => indices._1
      case other => other
    }))

    (swap(matrices._1), swap(matrices._2))
  }

  private def multiplyRow(matrices: MatrixTuple, row: Int, multiplier: Float): MatrixTuple = {
    def multiplyRow(list: List[List[Float]]): List[List[Float]] = {
      list.updated(row, list(row).map(cell => cell * multiplier))
    }

    (multiplyRow(matrices._1), multiplyRow(matrices._2))
  }

  private def addRowToRow(matrices: MatrixTuple, addedRow: Int, targetRow: Int, multiplier: Float): MatrixTuple  = {
    def addRowToRow(list: List[List[Float]]): List[List[Float]] = {
      list.updated(targetRow, list(targetRow) zip list(addedRow) map (tuple => tuple._1 + tuple._2 * multiplier))
    }

    (addRowToRow(matrices._1), addRowToRow(matrices._2))
  }

  private def identity(dimension: Int): List[List[Float]] = {
    require(dimension > 0, "Dimension must be greater than zero")
    List.range(0, dimension).map(row => List.tabulate(dimension){cell => if (cell == row) 1.0f else 0.0f})
  }
}



//object MatrixInverseOperation {
//  def apply(matrix: MatrixNxN[_]) = new MatrixInverseOperation(matrix).perform()
//}
