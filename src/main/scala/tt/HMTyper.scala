package tt

class HMTyper {
  type TypeResult = (Type, Seq[(Var, Type)])

  private val freeTypes: Iterator[Atom] = Stream.from(0).map(i => Atom(s"a$i")).iterator

  def infer(expr: LambdaExpression): Option[TypeResult] = {
    val freeVars = expr.freeVariables()
  }
}
