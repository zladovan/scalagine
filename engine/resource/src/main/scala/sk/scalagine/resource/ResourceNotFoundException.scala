package sk.scalagine.resource

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.09.14
 * Time: 22:52
 */
class ResourceNotFoundException(resource: ResourceLoader, cause: Throwable = null)
  extends RuntimeException(resource.toString, cause)
