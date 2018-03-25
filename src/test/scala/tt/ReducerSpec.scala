package tt

import org.scalatest.{FlatSpec, Matchers}

class ReducerSpec extends FlatSpec with Matchers {
  private def normalize: String => Lambda = LambdaParser(_).right.get.normalForm

  "Reducer" should "reduce (\\x.x) y" in {
    normalize("(\\x.x) y") shouldBe Variable("y")
  }

  "Reducer" should "reduce (\\y.\\x.y) x" in {
    normalize("(\\y.\\x.y) x") match {
      case Abstraction(bind, body) => bind shouldNot be (body)
      case _ => assert(false, "Wrong normal form")
    }
  }

}
