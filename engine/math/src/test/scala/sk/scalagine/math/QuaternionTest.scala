package sk.scalagine.math

import org.scalatest.{Matchers, WordSpec}
import java.lang.Math.sqrt
import sk.scalagine.math.transformation.Rotation
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 26.6.2014
 * Time: 21:53
 */
@RunWith(classOf[JUnitRunner])
class QuaternionTest extends WordSpec with Matchers with PlusMinusMatchers {
  private val quaternion1: Quaternion = Quaternion(1, 3, 4, 3)
  private val quaternion2: Quaternion = Quaternion(4, 3.9f, -1, -3)

  "Quaternion" should {
    "have conjugate form" in {
      quaternion1.conjugate should equal(Quaternion(1, -3, -4, -3))
    }

    "have norm" in {
      quaternion1.norm should equal(sqrt(35).asInstanceOf[Float])
    }

    "have normalized form" in {
      val norm: Float = sqrt(35).asInstanceOf[Float]
      quaternion1.normalize should equal(Quaternion(1 / norm, 3 / norm, 4 / norm, 3 / norm))
    }

    "have inversed form" in {
      val normSqrt: Float = 35
      quaternion1.inverse should equal(Quaternion(1 / normSqrt, 3 / normSqrt, 4 / normSqrt, 3 / normSqrt) +- 1e-5f)
    }

    "provide operator" which {
        "adds another quaternion" in {
            quaternion1 + quaternion2 should equal(Quaternion(5, 6.9f, 3, 0))
        }

        "subtracts another quaternion" in {
          quaternion1 - quaternion2 should equal(Quaternion(-3, -0.9f, 5, 6) +- 1e-5f)
        }

        "multiplies quaternion by another quaternion" in {
          quaternion1 * quaternion2 should equal(Quaternion(5.3f, 6.9f, 35.7f, -9.6f) +- 1e-5f)
        }

        "multiplies quaternion by scalar" in {
          quaternion1 * 2 should equal(Quaternion(2, 6, 8, 6))
        }

        "creates dot product with another quaternion" in {
          quaternion1 dot quaternion2  should equal(2.7f +- 1e-5f)
        }
    }

    "be able to rotate vector around axis" in {
      val angle: Float = Math.PI.asInstanceOf[Float]
      val axis: Vector3 = Vector3(1, 1, 0)
      val rotationQuaternion = Quaternion.Rotation(axis, angle)
      val rotationMatrix = Rotation(axis, angle)
      val vectorToRotate: Vector3 = Vector3(1, 2, 3)
      val vectorToRotate4: Vector4 = Vector4(vectorToRotate.x, vectorToRotate.y, vectorToRotate.z, 1)

      val rotatedViaQuaternion: Vector3 = (rotationQuaternion * vectorToRotate * rotationQuaternion.conjugate).ijk
      val rotatedViaMatrix4: Vector4 = rotationMatrix * vectorToRotate4
      val rotatedViaMatrix: Vector3 = Vector3(rotatedViaMatrix4.x, rotatedViaMatrix4.y, rotatedViaMatrix4.z)

      rotatedViaQuaternion should equal(rotatedViaMatrix +- 1e-5f)
    }
  }

  implicit def vector3ToQuaternion(vector: Vector3): Quaternion = Quaternion(vector)
}
