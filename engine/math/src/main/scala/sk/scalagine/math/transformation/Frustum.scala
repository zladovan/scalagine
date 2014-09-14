package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:46
 */
object Frustum {

  def apply(leftRight: (Float, Float), bottomTop: (Float, Float), zNearFar: (Float, Float)): Matrix4x4
    = frustum(leftRight, bottomTop, zNearFar)

  private def frustum(leftRight: (Float, Float), bottomTop: (Float, Float), zNearFar: (Float, Float)): Matrix4x4  = {
    val left = leftRight._1;    val right = leftRight._2
    val bottom = bottomTop._1;  val top = bottomTop._2
    val zNear = zNearFar._1;    val zFar = zNearFar._2

    require(zNear > 0 && zFar >= 0, "zNear and zFar must be positive, and zNear > 0")
    require(left != right && bottom != top, "top,bottom and left,right must not be equal")

    val zNear2 = 2.0f * zNear
    val dx = right - left
    val dy = top - bottom
    val dz = zFar - zNear

    Matrix4x4(
      zNear2 / dx,    0,              (right + left) / dx,            0,
      0,              zNear2 / dy,    (top + bottom) / dy,            0,
      0,              0,              -1.0f * ( zFar + zNear) / dz,  -2.0f *( zFar * zNear) / dz,
      0,              0,              -1,                             0)
  }
}
