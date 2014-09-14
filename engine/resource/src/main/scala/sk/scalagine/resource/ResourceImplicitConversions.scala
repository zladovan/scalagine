package sk.scalagine.resource

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 9/6/14
 * Time: 6:33 PM
 */
object ResourceImplicitConversions {
  implicit final def resourceToTextResource(resource: ResourceLoader): TextResource = new TextResource(resource)
}
