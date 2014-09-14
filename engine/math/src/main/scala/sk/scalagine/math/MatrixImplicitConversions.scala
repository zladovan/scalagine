package sk.scalagine.math

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.09.14
 * Time: 23:15
 */
object MatrixImplicitConversions {

  final implicit def matrixToMatrix2x2(m: Matrix[Vector2, Vector2]): Matrix2x2 =
    new Matrix2x2(
      m.data(0)(0), m.data(0)(1),
      m.data(1)(0), m.data(1)(1))

  final implicit def matrixToMatrix3x3(m: Matrix[Vector3, Vector3]): Matrix3x3 =
    new Matrix3x3(
      m.data(0)(0), m.data(0)(1), m.data(0)(2),
      m.data(1)(0), m.data(1)(1), m.data(1)(2),
      m.data(2)(0), m.data(2)(1), m.data(2)(2))

  final implicit def matrixToMatrix4x4(m: Matrix[Vector4, Vector4]): Matrix4x4 =
    new Matrix4x4(
      m.data(0)(0), m.data(0)(1), m.data(0)(2), m.data(0)(3),
      m.data(1)(0), m.data(1)(1), m.data(1)(2), m.data(1)(3),
      m.data(2)(0), m.data(2)(1), m.data(2)(2), m.data(2)(3),
      m.data(3)(0), m.data(3)(1), m.data(3)(2), m.data(3)(3))
}
