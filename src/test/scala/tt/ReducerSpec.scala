package tt

import org.scalatest.{FlatSpec, Matchers}

class ReducerSpec extends FlatSpec with Matchers {
  private def normalize: String => Lambda = LambdaParser(_).right.get.normalForm

  private def infer: String => Type = LambdaParser(_).flatMap(Typer(_).toRight(NoType())).right.get

  "Reducer" should "reduce (\\x.x) y" in {
    normalize("(\\x.x) y") shouldBe Variable("y")
  }

  "Reducer" should "reduce (\\y.\\x.y) x" in {
    normalize("(\\y.\\x.y) x") match {
      case Abstraction(bind, body) => bind shouldNot be (body)
      case _ => assert(false, "Wrong normal form")
    }
  }

  "Inferer" should "infer (\\x.x)" in {
    infer("\\x.x") match {
      case Arrow(Atom(x), Atom(y)) => x shouldBe y
      case _ => assert(false, "Incorrect inference")
    }
  }

  "Inferer" should "infer (\\x.x) x" in {
    infer("(\\x.x) x") match {
      case Atom(_) =>
      case _ => assert(false, "Incorrect inference")
    }
  }

}
