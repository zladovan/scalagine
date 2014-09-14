package sk.scalagine.math

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 25.5.2014
 * Time: 12:04
 */
class InverseMatrixNotExistsException(matrix: MatrixNxN[_])
  extends RuntimeException("Non invertible matrix: " + matrix)
