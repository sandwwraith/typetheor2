package tt

import org.parboiled2._

import scala.io.Source
import scala.util.{Failure, Success}

class LambdaParser(val input: ParserInput) extends Parser {

  def line: Rule1[Lambda] = rule { expr ~ EOI }

  private def expr: Rule1[Lambda] = rule { application | atom }

  private def application: Rule1[Lambda] = rule { atom ~ oneOrMore(" " ~ atom ~> Application) }

  private def atom: Rule1[Lambda] = rule { variable | abstraction | brackets }

  private def variable: Rule1[Variable] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(varName)) ~> Variable
  }

  private def brackets: Rule1[Lambda] = rule { "(" ~ expr ~ ")" }

  private def abstraction: Rule1[Lambda] = rule { "\\" ~ variable ~ wspStr(".") ~ expr ~> Abstraction }

  private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }

  private def varName = rule { CharPredicate.LowerAlpha | CharPredicate.Digit | CharPredicate(''') }
}

object LambdaParser {
  def apply(s: String): Either[ParsingException, Lambda] = {
    val parser = new LambdaParser(s)
    parser.line.run() match {
      case Success(l) => Right(l)
      case Failure(ex) => Left(ParsingException(ex.asInstanceOf[ParseError].format(parser)))
    }
  }
}
