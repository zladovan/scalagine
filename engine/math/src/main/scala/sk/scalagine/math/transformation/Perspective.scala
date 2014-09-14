package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:47
 */
object Perspective {

  def apply(fieldOfViewAngleYInDegrees: Float, aspectRatioX: Float, zNearFar: (Float, Float)): Matrix4x4 =
    perspective(fieldOfViewAngleYInDegrees, aspectRatioX, zNearFar)

  def perspective(fieldOfViewAngleYInDegrees: Float, aspectRatioX: Float, zNearFar: (Float, Float)): Matrix4x4 = {
    val top = (Math.tan(fieldOfViewAngleYInDegrees * Math.PI / 360.0f) * zNearFar._1).asInstanceOf[Float]
    val bottom = -1.0f * top
    val left = aspectRatioX * bottom
    val right = aspectRatioX * top

    Frustum((left, right), (bottom, top), zNearFar)
  }
}
