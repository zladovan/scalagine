package sk.scalagine.resource

import java.io.{ByteArrayInputStream, FileInputStream, File, InputStream}
import resource._
import java.nio.charset.{StandardCharsets, Charset}
import scala.Some
import java.net.URL

import scala.util.Try

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 9/6/14
 * Time: 6:33 PM
 */
abstract class Resource[ContentType](resourceLoader: ResourceLoader) {

  override def toString: String = resourceLoader.toString

  def content: ContentType
}

object Resource {
  private val EncodingForStringResource = StandardCharsets.UTF_8
  private val MaxCharactersPerNameShortcutForStringResource = 25

  def fromClassPath(pathUnderContext: String, contextRootClass: Class[_]): ResourceLoader = {

    val name: String = (Option(contextRootClass.getPackage) match {
      case Some(p) => p.getName + "."
      case None => ""
    }) + pathUnderContext.replace("/", ".")

    new ResourceLoader(name, () => contextRootClass.getResourceAsStream(pathUnderContext))
  }

  def fromFile(file: File): ResourceLoader =
    new ResourceLoader(file.getAbsolutePath, () => new FileInputStream(file))

  def fromURL(url: URL): ResourceLoader =
    new ResourceLoader(url.toString, () => url.openStream())

  def fromString(name: String, string: String): ResourceLoader =
    new ResourceLoader(
      name,
      () => new ByteArrayInputStream(string.getBytes(EncodingForStringResource)))

  def fromString(string: String): ResourceLoader = {
    // todo move into some helper
    def shortcut(string: String): String = {
      val stringLength = string.length

      string
        .substring(
          0,
          // todo create clamp function into some helper
          if (stringLength > MaxCharactersPerNameShortcutForStringResource)
            MaxCharactersPerNameShortcutForStringResource
          else
            string.length)
    }

    fromString(shortcut(string), string)
  }
}