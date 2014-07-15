package sk.scalagine.math

import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalautils.TripleEqualsSupport.Spread
import org.scalautils.Prettifier
import sk.scalagine.math

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 11.5.2014
 * Time: 21:55
 */
trait PlusMinusMatchers {

  trait ContainsDataList[U] {
    def dataList(): List[Float]
  }

  case class DataListSpread[T <: ContainsDataList[_]] (containsDataList: T, tolerance: Float){
    val spreads = containsDataList.dataList().map(cell => Spread(cell, tolerance))

    def isWithin(anotherContainsDataList: T): Boolean = {
      spreads
        .zip(anotherContainsDataList.dataList())
        .forall(spreadAndCell => spreadAndCell._1.isWithin(spreadAndCell._2))
    }

    def ===(n: T): Boolean = isWithin(n)
    def !==(n: T): Boolean = !isWithin(n)

    override def toString: String = Prettifier.default(containsDataList) + " +- " + Prettifier.default(tolerance)
  }

  def equal[T <% ContainsDataList[T]](dataListSpread: DataListSpread[ContainsDataList[T]]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          dataListSpread.isWithin(left),
          "{0} did not equal {1} plus or minus {2} on each component",
          "{0} equaled {1} plus or minus {2} on each component",
          Vector(left, dataListSpread.containsDataList, dataListSpread.tolerance)
        )
      }

      override def toString(): String = "equal (" + Prettifier.default(dataListSpread) + ")"
    }
  }

  final class ContainsDataListPlusOrMinusWrapper[T <: ContainsDataList[_]](pivot: T) {

    def +-(tolerance: Float): DataListSpread[T] = {
      if (tolerance < 0)
        throw new IllegalArgumentException(tolerance.toString + " passed to +- was zero or negative. " +
          "Must be a positive non-zero number.")

      DataListSpread(pivot, tolerance)
    }
  }

  implicit def convertVectorToContainsDataList[T <: math.Vector[_]](vector: T): ContainsDataList[T]
    = new ContainsDataList[T] {
      override def dataList(): List[Float] = vector.data
    override def toString: String = vector.toString
  }

  implicit def convertVectorToPlusMinusWrapper[T <: math.Vector[_]](vector: T): ContainsDataListPlusOrMinusWrapper[ContainsDataList[T]]
    = new ContainsDataListPlusOrMinusWrapper[ContainsDataList[T]](convertVectorToContainsDataList(vector))


  implicit def convertMatrixToContainsDataList[T <: Matrix[_,_]](matrix: T): ContainsDataList[T]
    = new ContainsDataList[T] {
        override def dataList(): List[Float] = matrix.data.flatMap(row => row)
        override def toString: String = matrix.toString
    }

  implicit def convertMatrixToPlusMinusWrapper[T <: Matrix[_,_]](matrix: T): ContainsDataListPlusOrMinusWrapper[ContainsDataList[T]]
    = new ContainsDataListPlusOrMinusWrapper[ContainsDataList[T]](convertMatrixToContainsDataList(matrix))

  implicit def convertQuaternionToContainsDataList(quaternion: Quaternion): ContainsDataList[Quaternion]
    = new ContainsDataList[Quaternion] {
    override def dataList(): List[Float] = quaternion.s :: quaternion.ijk.data
    override def toString: String = quaternion.toString
  }

  implicit def convertQuaternionToPlusMinusWrapper(quaternion: Quaternion): ContainsDataListPlusOrMinusWrapper[ContainsDataList[Quaternion]]
    = new ContainsDataListPlusOrMinusWrapper[ContainsDataList[Quaternion]](convertQuaternionToContainsDataList(quaternion))
}
