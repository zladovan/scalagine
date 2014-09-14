package sk.scalagine.resource

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import sk.scalagine.resource.ResourceImplicitConversions._

import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: zladovan
 * Date: 9/7/14
 * Time: 5:42 PM
 */
@RunWith(classOf[JUnitRunner])
class TextResourceTest extends WordSpec with Matchers {

  "A Text resource" when {

    "loaded successfully" should {

      val contentChecker = new ContentChecker(Resource.fromClassPath("test-resource.txt", getClass).content)

      "contain expected line count" in
        contentChecker
          .checkWithSingleGroup(
            "(?s).*It contains (\\d+) lines.*".r,
            contentChecker.lineCount.toString)

      "contain non-ASCII characters" in
        contentChecker
          .checkWithSingleGroup(
            "(?s).*It contains some non-ASCII characters (.*?)\\..*".r,
            "ľščťžýáíé")

      "contain expected epmty line count" in
        contentChecker
          .checkWithSingleGroup(
            "(?s).*It contains (\\d+) empty lines below this line.*".r,
            contentChecker.emptyLineCount.toString)



      "contain tab at start of line" in
        contentChecker
          .lineByPattern(".*It contains tab at start of this line.*".r)
            .startsWith("\t") should equal(true)

    }

  }

  private class ContentChecker(content: String){

    def checkWithSingleGroup(pattern: Regex, expectedInGroup: String) = {
      Option(content).collect({
        case pattern(foundInGroup) => foundInGroup
      }) should equal (Some(expectedInGroup))
    }

    def lineCount: Int = content.lines.length
    def emptyLineCount: Int = content.lines.count(line => line.isEmpty)

    def lineByPattern(pattern: Regex): String = {
      content.lines.find(line => pattern.pattern.matcher(line).matches()).getOrElse("")
    }
  }

}
