package tt

import org.parboiled2._

import scala.util.{Failure, Success}

class LambdaParser(val input: ParserInput) extends Parser {

  def lambda: Rule1[Lambda] = rule { expr ~ EOI }

  def lambdaExpr: Rule1[LambdaExpression] = rule { letExpr ~ EOI }

  private def letExpr: Rule1[LambdaExpression] = rule { ("let" ~ variable ~ "=" ~ letExpr ~ "in" ~ letExpr) ~> Substitution }

  private def expr: Rule1[Lambda] = rule { application | atom }

  private def application: Rule1[Lambda] = rule { atom ~ oneOrMore(" " ~ atom ~> Application) }

  private def atom: Rule1[Lambda] = rule { variable | abstraction | brackets }

  private def variable: Rule1[Variable] = rule { capture(CharPredicate.LowerAlpha ~ zeroOrMore(varName)) ~> Variable }

  private def brackets: Rule1[Lambda] = rule { "(" ~ expr ~ ")" }

  private def abstraction: Rule1[Lambda] = rule { "\\" ~ variable ~ wspStr(".") ~ expr ~> Abstraction }

  private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }

  private def varName = rule { CharPredicate.LowerAlpha | CharPredicate.Digit | CharPredicate(''') }
}

object LambdaParser {
  def apply(s: String): Either[ParsingException, Lambda] = {
    val parser = new LambdaParser(s)
    parser.lambda.run() match {
      case Success(l) => Right(l)
      case Failure(ex) => Left(ParsingException(ex.asInstanceOf[ParseError].format(parser)))
    }
  }
}

object LambdaExprParser {
  def apply(s: String): Either[ParsingException, LambdaExpression] = {
    val parser = new LambdaParser(s.replaceAll(" ", ""))
    parser.lambdaExpr.run() match {
      case Success(l) => Right(l)
      case Failure(ex) => Left(ParsingException(ex.asInstanceOf[ParseError].format(parser)))
    }
  }
}
