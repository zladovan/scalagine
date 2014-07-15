package sk.scalagine.math.transformation

import org.scalatest.{Matchers, WordSpec}
import sk.scalagine.math.{Matrix4x4, PlusMinusMatchers, Vector3}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 31.5.2014
 * Time: 1:23
 */
@RunWith(classOf[JUnitRunner])
class TransformationsTest extends WordSpec with Matchers with PlusMinusMatchers {

  "Transformations" should {

    "provide identity 4x4" in {
      Identity() should equal(Matrix4x4( 1, 0, 0, 0,
                                         0, 1, 0, 0,
                                         0, 0, 1, 0,
                                         0, 0, 0, 1))
    }

    "provide lootAt matrix" in {
      LookAt( Vector3(0, 0, 2),
              Vector3(0, 0, -1),
              Vector3(0, 1, 0)) should equal(Matrix4x4( 1,  0,  0,  0,
                                                        0,  1,  0,  0,
                                                        0,  0,  1, -2,
                                                        0,  0,  0,  1))
    }

    "provide frustum  projection matrix" in {
      val width: Int = 320
      val height: Int = 480
      val ratio: Float = width.asInstanceOf[Float] / height
      Frustum((-ratio, ratio), (-1, 1), (1, 10)) should equal(
        Matrix4x4( 1.5f,   0,    0,     0,
                   0,      1,    0,     0,
                   0,      0,   -1.2f, -2.2f,
                   0,      0,   -1,     0)    +- 1e-1f)


    }

    "provide orthographic projection matrix" in {
      val width: Int = 320
      val height: Int = 480
      val ratio: Float = width.asInstanceOf[Float] / height

      Ortho((-ratio, ratio), (-1, 1), (1, 10)) should equal(
        Matrix4x4(
          1.51f,  0.0f,   0.0f,  0.0f,
           0.0f,  1.0f,   0.0f,  0.0f,
           0.0f,  0.0f, -0.22f, 1.22f,
           0.0f,  0.0f,   0.0f,  1.0f)  +- 1e-2f)
    }

    "provide rotation matirix" which {
      "represents rotation around arbitrary axis" in {
          Rotation(Vector3(-1/3f, 2/3f, 2/3f), -1.29154365f) should equal(
            Matrix4x4( 0.36f,   0.48f,   -0.8f,   0,
                      -0.8f,    0.6f,     0,      0,
                       0.48f,   0.64f,    0.6f,   0,
                       0,       0,        0,      1) +- 1e-2f
          )
      }

      "represents rotation around x axis" in {
          Rotation.x(2 * Math.PI.asInstanceOf[Float]) should equal(
            Matrix4x4(  1,     0,     0,     0,
                        0,     1,     0,     0,
                        0,     0,     1,     0,
                        0,     0,     0,     1) +- 1e-5f
          )

      }

      "represents rotation around y axis" in {
        Rotation.y(3 * Math.PI.asInstanceOf[Float]) should equal(
          Matrix4x4( -1,     0,     0,     0,
                      0,     1,     0,     0,
                      0,     0,    -1,     0,
                      0,     0,     0,     1) +- 1e-5f
        )
      }

      "represents rotation around z axis" in {
        Rotation.z(3 * Math.PI.asInstanceOf[Float]) should equal(
          Matrix4x4( -1,     0,     0,     0,
                      0,    -1,     0,     0,
                      0,     0,     1,     0,
                      0,     0,     0,     1) +- 1e-5f
        )
      }
    }

    "provide scale matrix" which {
      "represents uniform scale" in {
        Scale(5) should equal (Matrix4x4( 5,  0,  0,  0,
                                          0,  5,  0,  0,
                                          0,  0,  5,  0,
                                          0,  0,  0,  1)
        )
      }

      "represents nonuniform scale" in {
        Scale(1, 2, 3) should equal (Matrix4x4( 1,  0,  0,  0,
                                                0,  2,  0,  0,
                                                0,  0,  3,  0,
                                                0,  0,  0,  1))
      }
    }

    "provide translation matrix" in {
      Translation(1, 2, 3) should equal(Matrix4x4(  0,  0,  0,  1,
                                                    0,  0,  0,  2,
                                                    0,  0,  0,  3,
                                                    0,  0,  0,  1))
    }

  }

}
