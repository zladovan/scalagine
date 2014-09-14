package sk.scalagine.math.transformation

import sk.scalagine.math.{Matrix4x4, Vector3}

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:43
 */
object LookAt {

  def apply(eye: Vector3, target: Vector3, up: Vector3): Matrix4x4 = lookAt(eye, target, up)

  private def lookAt(eye: Vector3, target: Vector3, up: Vector3): Matrix4x4 = {
    val z = (target - eye).normalize
    val x = (z cross up).normalize
    val y = x cross z
    val minusEye = eye * -1
    val t = Vector3(x dot minusEye, y dot minusEye, (z * -1) dot minusEye)

    Matrix4x4(
       x.x,    x.y,    x.z,    t.x,
       y.x,    y.y,    y.z,    t.y,
      -z.x,   -z.y,   -z.z,    t.z,
       0,      0,      0,      1)
  }
}