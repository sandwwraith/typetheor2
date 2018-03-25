package tt

import org.scalatest.{FlatSpec, Matchers}
import tt._

class ParserSpec extends FlatSpec with Matchers {

  private def mustBeParsed(result: Either[ParsingException, Lambda]) = result match {
    case Right(_) =>
    case Left(f) => assert(false, s"Parser must completed normally, but emitted error: $f")
  }

  "Lambda parser" should "parse \\x.\\y.x" in {
    mustBeParsed(LambdaParser("\\x.\\y.xab''"))
  }

  "Lambda parser" should "fail on \\X.\\y.x" in {
    LambdaParser("\\X.\\y.xab''") match {
      case Left(_) =>
      case Right(l) => assert(false, s"Parser must fail, but have parsed: $l")
    }
  }

}
