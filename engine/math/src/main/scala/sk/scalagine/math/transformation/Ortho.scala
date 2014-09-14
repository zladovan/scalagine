package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 13:23
 */
object Ortho {

  def apply(leftRight: (Float, Float), bottomTop: (Float, Float), zNearFar: (Float, Float)): Matrix4x4 = {
    val left = leftRight._1;    val right = leftRight._2
    val bottom = bottomTop._1;  val top = bottomTop._2
    val zNear = zNearFar._1;    val zFar = zNearFar._2

    require(zNear > 0 && zFar >= 0, "zNear and zFar must be positive, and zNear > 0")
    require(left != right && bottom != top, "top,bottom and left,right must not be equal")

    Matrix4x4(
      2.0f / (right - left),                   0.0f,                   0.0f,  (left + right) / (left - right),
                       0.0f,  2.0f / (top - bottom),                   0.0f,  (bottom + top) / (bottom - top),
                       0.0f,                   0.0f,  2.0f / (zNear - zFar),  (zNear + zFar) / (zFar - zNear),
                       0.0f,                   0.0f,                   0.0f,                             1.0f)
  }
}
