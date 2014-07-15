package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:37
 */
object Translation {
  def apply(x: Float, y: Float, z: Float): Matrix4x4 = translation(x, y, z)
  def x(amount: Float) = translation(amount, 0, 0)
  def y(amount: Float) = translation(0, amount, 0)
  def z(amount: Float) = translation(0, 0, amount)

  private def translation(x: Float, y: Float, z: Float): Matrix4x4 = Matrix4x4(
    0,      0,      0,      x,
    0,      0,      0,      y,
    0,      0,      0,      z,
    0,      0,      0,      1
  )
}
