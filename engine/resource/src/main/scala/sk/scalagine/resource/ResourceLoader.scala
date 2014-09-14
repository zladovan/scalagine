package sk.scalagine.resource

import java.io.InputStream

import resource._

import scala.util.control.NonFatal
import scala.util.{Success, Failure, Try}

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 13.09.14
 * Time: 23:00
 */
class ResourceLoader(val name: String, loadContent: () => InputStream) {

  // todo error handling (return Try[ContentType] ?)
  def openAndCloseInputStream[ContentType](readInputStream: InputStream => ContentType): ContentType =
    Try(Option(loadContent())) match {
      case Success(Some(inputStream)) => managed(inputStream) acquireAndGet (is => readInputStream(is))
      case Success(None) => throw new ResourceNotFoundException(this)
      case Failure(e) => throw new ResourceNotFoundException(this, e)
    }


  override def toString: String = name
}
