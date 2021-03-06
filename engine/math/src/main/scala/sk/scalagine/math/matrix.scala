package sk.scalagine.math

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 4.10.2013
 * Time: 15:37
 * To change this template use File | Settings | File Templates.
 */
class Matrix[RowDimension <: Vector[RowDimension], ColumnDimension <: Vector[ColumnDimension]] protected (
    val data: List[List[Float]]) {

  //todo check data size
  //require(data.length == ???)

  def +(matrix: Matrix[RowDimension, ColumnDimension]): Matrix[RowDimension, ColumnDimension] =
    add(matrix, cell => cell)

  def -(matrix: Matrix[RowDimension, ColumnDimension]): Matrix[RowDimension, ColumnDimension] =
    add(matrix, cell => -cell)

  def *[NewColumnDimension <: Vector[NewColumnDimension]](
      matrix: Matrix[ColumnDimension, NewColumnDimension]): Matrix[RowDimension, NewColumnDimension] =
    new Matrix[RowDimension, NewColumnDimension](mul(matrix.data))

  def *(number: Float): Matrix[RowDimension, ColumnDimension] =
    new Matrix[RowDimension, ColumnDimension](data map (row => row map (cell => cell * number)))

  def *(vector: ColumnDimension): ColumnDimension =
    vector.newFromData(mul(vector.data.map(coordinate => List(coordinate))).transpose.head)


  def transpose: Matrix[ColumnDimension, RowDimension] = new Matrix[ColumnDimension, RowDimension](data.transpose)

  private def add(
      matrix: Matrix[RowDimension, ColumnDimension],
      multiplier: Float => Float): Matrix[RowDimension, ColumnDimension] =
    new Matrix[RowDimension, ColumnDimension](
      // todo how to format ??
      data zip matrix.data map { row =>
        row._1 zip row._2 map { cell =>
          cell._1 + multiplier(cell._2)
        }
      })

  private def mul(data: List[List[Float]]): List[List[Float]] =
    for (row <- this.data)
      yield for (col <- data.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)


  // todo: nice output
  override def toString: String = data.toString()

  def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix[RowDimension, ColumnDimension]]

  override def equals(other: Any): Boolean = other match {
    case that: Matrix[RowDimension, ColumnDimension] => (that canEqual this) && data.equals(that.data)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(data)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class MatrixNxN[Dimension <: Vector[Dimension]] protected (dataList: List[List[Float]])
  extends Matrix[Dimension, Dimension](dataList){

  def inverse: MatrixNxN[Dimension] =
    new InverseOperation(this).perform(resultData => new MatrixNxN[Dimension](resultData))

  def determinant: Float = new DeterminantOperation(this).perform()
}

case class Matrix2x2( r1c1: Float, r1c2: Float,
                      r2c1: Float, r2c2: Float)
  extends MatrixNxN[Vector2](
    List(
      List(r1c1, r1c2),
      List(r2c1, r2c2)))

case class Matrix2x3( r1c1: Float, r1c2: Float, r1c3: Float,
                      r2c1: Float, r2c2: Float, r2c3: Float)
  extends Matrix[Vector2, Vector3](
    List(
      List(r1c1, r1c2, r1c3),
      List(r2c1, r2c2, r2c3)))

case class Matrix3x2( r1c1: Float, r1c2: Float,
                      r2c1: Float, r2c2: Float,
                      r3c1: Float, r3c2: Float)
  extends Matrix[Vector3, Vector2](
    List(
      List(r1c1, r1c2),
      List(r2c1, r2c2),
      List(r3c1, r3c2)))

case class Matrix3x3( r1c1: Float, r1c2: Float, r1c3: Float,
                      r2c1: Float, r2c2: Float, r2c3: Float,
                      r3c1: Float, r3c2: Float, r3c3: Float)
  extends MatrixNxN[Vector3](
    List(
      List(r1c1, r1c2, r1c3),
      List(r2c1, r2c2, r2c3),
      List(r3c1, r3c2, r3c3)))

case class Matrix4x4( r1c1: Float, r1c2: Float, r1c3: Float, r1c4: Float,
                      r2c1: Float, r2c2: Float, r2c3: Float, r2c4: Float,
                      r3c1: Float, r3c2: Float, r3c3: Float, r3c4: Float,
                      r4c1: Float, r4c2: Float, r4c3: Float, r4c4: Float)
  extends MatrixNxN[Vector4](
    List(
      List(r1c1, r1c2, r1c3, r1c4),
      List(r2c1, r2c2, r2c3, r2c4),
      List(r3c1, r3c2, r3c3, r3c4),
      List(r4c1, r4c2, r4c3, r4c4)))
