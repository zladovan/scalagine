package sk.scalagine.math

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 24.5.2014
 * Time: 21:56
 */
class DeterminantOperation(matrix: MatrixNxN[_]) {
  private type MatrixData = List[List[Float]]


  def perform(): Float = {
    List.range(0, matrix.data.length)
      .zip(clearBelowDiagonal(matrix.data))
      .foldRight(1f)((rowData, a) => a * rowData._2(rowData._1))
  }


  private def clearBelowDiagonal(matrixData: MatrixData): MatrixData = {

    @tailrec def clearBelowDiagonal(matrixData: MatrixData, column: Int): MatrixData = column match {
      case index if index == matrixData.length => matrixData
      case other => clearBelowDiagonal(clearColumnBelowDiagonal(matrixData, column), column + 1)
    }

    clearBelowDiagonal(matrixData, 0)
  }
  
  private def clearColumnBelowDiagonal(matrixData: MatrixData, column: Int): MatrixData = {

    @tailrec def clearColumnBelowDiagonal(matrixData: MatrixData, column: Int, row: Int): MatrixData = row match {
      case index if index == matrixData.length => matrixData
      case other => clearColumnBelowDiagonal(clearRow(matrixData, column, row), column, row + 1)
    }

    clearColumnBelowDiagonal(matrixData, column, column + 1)
  }

  private def clearRow(matrixData: MatrixData, column: Int, row: Int): MatrixData = {
    val multiplier = matrixData(row)(column) / matrixData(column)(column)

    matrixData.updated(
      row,
      matrixData(row) zip matrixData(column)  map (cell => cell._1 - cell._2 * multiplier))
  }
}
