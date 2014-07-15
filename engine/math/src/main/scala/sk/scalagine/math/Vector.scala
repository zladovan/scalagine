package sk.scalagine.math

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 4.10.2013
 * Time: 15:37
 * To change this template use File | Settings | File Templates.
 */
abstract class Vector[Self <: Vector[Self]](val data: List[Float]) {
  self: Self =>

  require(data.length == dimension, "Wrong number of vector elements. Expected: %d Given: %s".format(dimension, data))

  def dot(vector: Self ): Float = ((data zip vector.data) foldLeft 0.0f) {(a, b) => a + b._1 * b._2}
  def *(scalar: Float): Self = newInstance(data map {vi => vi * scalar})
  def /(scalar: Float): Self = newInstance(data map {vi => vi / scalar})
  def +(scalar: Float): Self = newInstance(data map {vi => vi + scalar})
  def +(vector: Self): Self = add(vector, {right => right})
  def -(scalar: Float): Self = this + -scalar
  def -(vector: Self): Self = add(vector, {right => -right})
  def length: Float = math.sqrt((data foldLeft 0.0f) {(a, b) => a + (b * b)}).floatValue
  def normalize: Self = this * (1 / length)
// todo vector power ^^ for even ^^^ for odd
//  def ^^(scalar: Float): Float = {
//    require(scalar % 2 == 0, "Even number of power is required")
//    if (scalar == 0)
//  }

  def newFromData(data: List[Float]): Self = newInstance(data)

  // abstract methods
  protected def newInstance(data: List[Float]): Self
  def dimension: Int

  // aliases
  def *(vector: Self): Float = dot(vector)

  private def add(vector: Self, multiplier: Float => Float): Self = {
    newInstance((data zip vector.data)  map {a => a._1 + multiplier(a._2)})
  }
}

case class Vector2(x: Float, y: Float) extends Vector[Vector2](List(x, y)) {
  protected def newInstance(data: List[Float]): Vector2 = Vector2(data(0), data(1))
  def dimension = 2
}

case class Vector3(x: Float, y: Float, z: Float) extends Vector[Vector3](List(x, y, z))  {
  protected def newInstance(data: List[Float]): Vector3 = Vector3(data(0), data(1), data(2))
  def dimension = 3
  def cross(vec: Vector3): Vector3 = Vector3(y * vec.z - z * vec.y, z * vec.x - x * vec.z, x * vec.y - y * vec.x)

  // aliases
  def ×(vector: Vector3): Vector3 = cross(vector)
}

case class Vector4(x: Float, y: Float, z: Float, w: Float) extends Vector[Vector4](List(x, y, z, w))  {
  protected def newInstance(data: List[Float]): Vector4 = Vector4(data(0), data(1), data(2), data(3))
  def dimension = 4
}







