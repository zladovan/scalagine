package sk.scalagine.math.transformation

import sk.scalagine.math.Matrix4x4

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.7.2014
 * Time: 12:50
 */
object Identity {
  // todo re-think, cannot be used simple as Identity (Identity() is needed)
  def apply(): Matrix4x4 = new Matrix4x4(
    1,      0,      0,      0,
    0,      1,      0,      0,
    0,      0,      1,      0,
    0,      0,      0,      1
  )

}
