package sk.scalagine.resource

import java.io.File
import java.net.URL

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 14.09.14
 * Time: 12:45
 */
@RunWith(classOf[JUnitRunner])
class ResourceLoaderTest extends WordSpec with Matchers {

  val CorrectResourcePathShort = "test-resource.txt"
  val CorrectResourcePathFull = "/sk/scalagine/resource/test-resource.txt"
  val IncorrectResourcePath = "test.txt"
  
  "A resource" when {

    "from class path" should {


      "be loaded successfully" when {

        "correct path and context class given" in
          ResourceChecker(Resource.fromClassPath(CorrectResourcePathShort, getClass))
            .expectExistence()


        "full path and any context class given" in
          ResourceChecker(Resource.fromClassPath(CorrectResourcePathFull, Matchers.getClass))
            .expectExistence()

      }

      "throw ResourceNotFoundException " when {

        "correct path and wrong context class given" in
          ResourceChecker(Resource.fromClassPath(CorrectResourcePathShort, Matchers.getClass))
            .expectResourceNotFoundException()


        "wrong path and correct context class given" in
          ResourceChecker(Resource.fromClassPath(IncorrectResourcePath, getClass))
            .expectResourceNotFoundException()


        "wrong path and wrong context class" in
          ResourceChecker(Resource.fromClassPath(IncorrectResourcePath, Matchers.getClass))
            .expectResourceNotFoundException()

      }

    }

    "from file" should {

      val ExistingFile = new File(getClass.getResource(CorrectResourcePathShort).toURI)
      val NotExistingFile = new File(IncorrectResourcePath)

      "be successfully loaded when existing file given" in
        ResourceChecker(Resource.fromFile(ExistingFile)).expectExistence()


      "throw ResourceNotFoundException when not existing file given" in
        ResourceChecker(Resource.fromFile(NotExistingFile))
          .expectResourceNotFoundException()

    }

    "from string" should {

      "be successfully loaded" when {

        "string with name given" in
          ResourceChecker(Resource.fromString("Test resource", "This is some resource text."))
            .expectExistence()


        "non empty string without name given" in
          ResourceChecker(Resource.fromString("This is some resource text")).expectExistence()

        "empty string without name given" in
          ResourceChecker(Resource.fromString("")).expectExistence()

      }

      "from url" should {
        val ExistingURL = getClass.getResource(CorrectResourcePathShort)
        val NonExistingURL = new URL("file://test")

        "be successfully loaded when existing url given" in
          ResourceChecker(Resource.fromURL(ExistingURL))
            .expectExistence()

        "throws ResourceNotFoundException when not existing url given" in
              ResourceChecker(Resource.fromURL(NonExistingURL))
                .expectResourceNotFoundException()

      }

    }

  }

  private case class ResourceChecker(resourceLoader: ResourceLoader){

    def expectExistence(): Unit =
      resourceLoader.openAndCloseInputStream(inputStream => Option(inputStream) should not equal None)

    def expectResourceNotFoundException(): Unit = intercept[ResourceNotFoundException] {
      expectExistence()
    }
  }

}
