package tt

import scala.io.Source

trait TaskRunner[R] {
  def run(args: Array[String], taskNumber: Int)
         (implicit action: String => Either[LambdaError, R], format: R => String = _.toString) {
    val input = if (args.isEmpty) s"task$taskNumber.in" else args(0)
    val fst = Source.fromFile(input) getLines() map (_.trim) filterNot (_.isEmpty) next

    action(fst) match {
      case Left(err) => System.err.print(s"$err \n")
      case Right(r) => println(format(r))
    }
  }

}

object Task1 extends App with TaskRunner[Lambda] {
  implicit def action(s: String): Either[LambdaError, Lambda] = LambdaParser(s).map(_.normalForm)

  run(args, 1)
}

object Playground extends App {
  //  val input = "(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\f.\\x.f (f (f x)))"
  //  val parsed = LambdaParser(input).right.get
  //  println(parsed.normalForm)
  FibTest.fib(5)
}
