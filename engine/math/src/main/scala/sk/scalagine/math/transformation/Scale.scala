package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:34
 */
object Scale {

  def apply(x: Float, y: Float, z: Float): Matrix4x4 = scale(x, y, z)

  def apply(factor: Float): Matrix4x4 = scale(factor, factor, factor)

  def x(factor: Float): Matrix4x4 = scale(factor, 1.0f, 1.0f)

  def y(factor: Float): Matrix4x4 = scale(1.0f, factor, 1.0f)

  def z(factor: Float): Matrix4x4 = scale(1.0f, 1.0f, factor)

  private def scale(x: Float, y: Float, z: Float): Matrix4x4 =
    Matrix4x4(
      x,      0,      0,      0,
      0,      y,      0,      0,
      0,      0,      z,      0,
      0,      0,      0,      1)
}

