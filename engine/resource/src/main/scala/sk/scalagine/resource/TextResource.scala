package sk.scalagine.resource

import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 9/6/14
 * Time: 6:33 PM
 */
class TextResource(resource: ResourceLoader) extends Resource[String](resource) {

  override def content: String =
    resource
      .openAndCloseInputStream(
        inputStream =>
          Source.fromInputStream(inputStream).mkString)

}
