package sk.scalagine.math

import org.scalatest.{Matchers, WordSpec}
import MatrixImplicitConversions._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 12.5.2014
 * Time: 21:25
 */
@RunWith(classOf[JUnitRunner])
class MatrixTest extends WordSpec with Matchers with PlusMinusMatchers {
  private val matrix2x3: Matrix2x3 = Matrix2x3(1, 2, 3,
                                               4, 5, 6)

  private val matrix3x2: Matrix3x2 = Matrix3x2(1, 2,
                                               3, 4,
                                               5, 6)

  private val matrix3x3: Matrix3x3 = Matrix3x3(1, 2, 3,
                                               4, 5, 6,
                                               7, 8, 9)

  private val matrix3x3Invertible = Matrix3x3( 2,  3,	 8,
                                               6,  0, -3,
                                              -1,	 3,	 2)

  "A matrix" should {

    "have correctly set corresponding coordinates" in {
      matrix3x3 should have (
        'r1c1(1), 'r1c2(2), 'r1c3(3),
        'r2c1(4), 'r2c2(5), 'r2c3(6),
        'r3c1(7), 'r3c2(8), 'r3c3(9)
      )
    }

    "have transposed form" in {
      matrix2x3.transpose should equal(Matrix3x2(1, 4,
                                                 2, 5,
                                                 3, 6))
    }

    "have inverse operation " which {
      "returns inversed form" in {

        //todo update equal or +- to be able handle implicitly converted matrix
        matrixToMatrix3x3(matrix3x3Invertible.inverse * matrix3x3Invertible) should equal(Matrix3x3(1, 0, 0,
                                                                                                    0, 1, 0,
                                                                                                    0, 0, 1)  +- 1e-5f)
      }

      "throws an exception if is not invertible" in {
        intercept[InverseMatrixNotExistsException] {
          matrix3x3.inverse
        }
      }
    }

    "have determinant" which {
      "can be zero" in {
        matrix3x3.determinant should equal(0)
      }

      "can be non zero" in {
        matrix3x3Invertible.determinant should equal(135)
      }
    }

    "provide operator" which {

      "muliplies matrix by a number" in {
        matrix3x3 * 10 should equal(Matrix3x3(10, 20, 30,
                                              40, 50, 60,
                                              70, 80, 90))
      }

      "multiplies matrix by vector" in {
        matrix3x3 * Vector3(1, 2, 3) should  equal(Vector3(14, 32, 50))
      }

      "multiplies matrix by another matrix" in {
        matrix2x3 * matrix3x2 should equal(Matrix2x2(22, 28,
                                                     49, 64))
      }

      "adds another matrix" in {
        matrix3x3 + matrix3x3 should equal(Matrix3x3( 2,  4,  6,
                                                      8, 10, 12,
                                                      14, 16, 18))
      }

      "subtracts another matrix" in {
        matrix3x3 - matrix3x3 should equal(Matrix3x3( 0, 0, 0,
                                                      0, 0, 0,
                                                      0, 0, 0))
      }
    }
  }
}
