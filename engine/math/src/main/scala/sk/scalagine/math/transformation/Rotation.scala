package sk.scalagine.math.transformation

import sk.scalagine.math.{Matrix4x4, Vector3}

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:33
 */
object Rotation {
  def apply(axis: Vector3, angleInRadians: Float): Matrix4x4 = rotation(axis, angleInRadians)
  def x(angleInRadians: Float):Matrix4x4 = rotationX(angleInRadians)
  def y(angleInRadians: Float):Matrix4x4 = rotationY(angleInRadians)
  def z(angleInRadians: Float):Matrix4x4 = rotationZ(angleInRadians)

  private def rotation(axis: Vector3, angleInRadians: Float): Matrix4x4 = {
    val axisNormalized = axis.normalize
    val c: Float = Math.cos(angleInRadians).asInstanceOf[Float]
    val omc: Float = 1 - c
    val s: Float = Math.sin(angleInRadians).asInstanceOf[Float]
    val x: Float = axisNormalized.x; val xPow2: Float = Math.pow(x, 2).asInstanceOf[Float]
    val y: Float = axisNormalized.y; val yPow2: Float = Math.pow(y, 2).asInstanceOf[Float]
    val z: Float = axisNormalized.z; val zPow2: Float = Math.pow(z, 2).asInstanceOf[Float]

    Matrix4x4(
      c + omc * xPow2,          omc * x * y - s * z,      omc * x * z + s * y,      0,
      omc * x * y + s * z,      c + omc * yPow2,          omc * y * z - s * x,      0,
      omc * x * z - s * y,      omc * y * z + s * x,      c + omc * zPow2,          0,
      0,                        0,                        0,                        1)
  }

  private def rotateAroundAxis(angleInRadians: Float, createMatrix: (Float, Float) => Matrix4x4): Matrix4x4 = {
    createMatrix(Math.cos(angleInRadians).asInstanceOf[Float], Math.sin(angleInRadians).asInstanceOf[Float])
  }

  private def rotationX(angleInRadians: Float): Matrix4x4 = rotateAroundAxis(angleInRadians, {(cos, sin) => Matrix4x4(
    1,      0,      0,      0,
    0,    cos,   -sin,      0,
    0,    sin,    cos,      0,
    0,      0,      0,      1
  )})

  private def rotationY(angleInRadians: Float): Matrix4x4 = rotateAroundAxis(angleInRadians, {(cos, sin) => Matrix4x4(
    cos,      0,    sin,      0,
      0,      1,      0,      0,
    -sin,     0,    cos,      0,
      0,      0,      0,      1
  )})

  private def rotationZ(angleInRadians: Float): Matrix4x4 = rotateAroundAxis(angleInRadians, {(cos, sin) => Matrix4x4(
    cos,   -sin,      0,      0,
    sin,    cos,      0,      0,
    0,      0,        1,      0,
    0,      0,        0,      1
  )})
}
