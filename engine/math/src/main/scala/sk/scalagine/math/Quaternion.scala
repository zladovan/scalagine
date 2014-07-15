package sk.scalagine.math

import java.lang.Math.{sqrt, pow, abs, sin, cos}

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 26.6.2014
 * Time: 21:29
 */
case class Quaternion(s: Float, ijk: Vector3) {
  val i = ijk.x
  val j = ijk.y
  val k = ijk.z

  def +(q: Quaternion): Quaternion = new Quaternion(s + q.s, ijk + q.ijk)
  def -(q: Quaternion): Quaternion = new Quaternion(s - q.s, ijk - q.ijk)
  def *(q: Quaternion): Quaternion = new Quaternion(s * q.s - ijk * q.ijk, q.ijk * s + ijk * q.s + ijk × q.ijk)
  def *(scalar: Float): Quaternion = new Quaternion(s * scalar, ijk * scalar)
  def /(scalar: Float): Quaternion = new Quaternion(s / scalar, ijk / scalar)
  def dot(q: Quaternion): Float = s * q.s + (ijk dot q.ijk)
  def conjugate: Quaternion = new Quaternion(s, ijk * -1)
  // todo vector pow move to vector
  def norm: Float = sqrt(pow(s, 2) + (ijk dot ijk)).asInstanceOf[Float]
  def normalize: Quaternion = this / norm
  def inverse: Quaternion = this / pow(norm, 2).asInstanceOf[Float]


  override def toString: String = {
    def partToString(ins: (Int, Float, String)): String = partToStringUntupled(ins._1, ins._2, ins._3)

    def partToStringUntupled(index: Int, number: Float, symbol: String): String = number match {
      case zero if zero == 0 => ""
      case nonZero => (if (number < 0) {sign(index, " -") + abs(number)} else {sign(index, " +") + number}) + symbol
    }

    def sign(index: Int, signSymbol: String): String = signSymbol + (if (index > 0) " " else "")


    s + (List.range(0, ijk.data.length), ijk.data, List("i", "j", "k")).zipped
        .foldLeft(""){(prev, indexAndNumberAndSymbol) => prev + partToString(indexAndNumberAndSymbol)}
  }
}

object Quaternion {
  def apply(s: Float, i: Float, j: Float, k: Float): Quaternion = new Quaternion(s, Vector3(i, j, k))
//  def apply[T <% Float](s: Float, ijk: (T, T, T)): Quaternion = Quaternion(s, ijk._1, ijk._2, ijk._3)
  def apply(s: Float): Quaternion = Quaternion(s, 0, 0, 0)
  def apply(i: Float, j: Float, k: Float): Quaternion = Quaternion(0, i, j, k)
  def apply(ijk: Vector3) = new Quaternion(0, ijk)
  //def apply(ijk: (Float, Float, Float)): Quaternion = Quaternion(0, ijk)

  // todo where to put rotation creation ?
  object Rotation {
    def apply(axis: Vector3, angleInRadians: Float): Quaternion = {
      val halfAngle: Float = angleInRadians / 2.0f
      Quaternion(cos(halfAngle).asInstanceOf[Float], axis.normalize * sin(halfAngle).asInstanceOf[Float])
    }
  }
}
