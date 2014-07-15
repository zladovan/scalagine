package sk.scalagine.math

import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 6.10.2013
 * Time: 12:26
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class VectorTest extends WordSpec with Matchers with PlusMinusMatchers {
  private val vector21 = Vector2(1, 2)
  private val vector22 = Vector2(3, 4)
  private val vector31 = Vector3(1, 2, 3)
  private val vector32 = Vector3(4, 5, 6)
  private val vector41 = Vector4(1, 2, 3, 4)
  private val vector42 = Vector4(5, 6, 7, 8)

  "A vector" should {

    "have correctly set corresponding coordinates" in {
      vector31 should have (
        'x(1),
        'y(2),
        'z(3)
      )
    }

    "have length" in {
      vector31.length should equal (3.7416f +- 1e-4f)
    }

    "have normalized form" in {
      vector31.normalize should equal(Vector3(0.2672f, 0.53452f, 0.80178f) +- 1e-4f)
    }

    "provide operator" which {

      "adds another vector" in {
        vector31 + vector32 should equal(Vector3(5, 7, 9))
      }

      "adds a scalar" in {
        vector31 + 10 should equal(Vector3(11, 12, 13))
      }

      "subtracts another vector" in {
        vector31 - vector32 should equal(Vector3(-3, -3, -3))
      }

      "subtracts a scalar" in {
        vector31 - 10 should equal(Vector3(-9, -8, -7))
      }

      "multiplies vector by a scalar" in {
        vector31 * 10 should equal(Vector3(10, 20, 30))
      }

      "divides vector by a scalar" in {
        vector31 / 10 should equal(Vector3(0.1f, 0.2f, 0.3f))
      }

      "creates dot product with another vector" in {
        vector31 * vector32 should equal(32)
      }

      "creates cross product with another vector" in {
        vector31 × vector32 should equal(Vector3(-3, 6, -3))
      }

//      "creates even power as a scalar" in {
//        vector31 ^^ 2 should equal (14)
//      }
//
//      "create odd power as a vector" in {
//        vector31 ^^^ 3 should equal (Vector3(14, 28, 42))
//      }
    }

  }


}
