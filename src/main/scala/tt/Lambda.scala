package tt


import scala.collection.immutable.Set
import scala.collection.{mutable => m}

sealed abstract class Lambda {

  private type LL = Lambda => Lambda

  def freeVariables(scope: Set[Variable] = Set()): Set[Variable] = this match {
    case Abstraction(v, b) => b.freeVariables(scope + v)
    case Application(x, y) => x.freeVariables(scope) ++ y.freeVariables(scope)
    case v@Variable(_) if !scope.contains(v) => Set(v)
    case _ => Set()
  }

  def substitute(v: Variable, e: Lambda, scope: Set[Variable] = Set()): Lambda = this match {
    case Variable(_) if v == this => e
    case v@Variable(_) => v
    case Application(x, y) => Application(x.substitute(v, e, scope), y.substitute(v, e, scope))
    case Abstraction(bind, body) if body.freeVariables().contains(v) =>
      val newBind = v/
      val newBody = body.substitute(bind, newBind)
      Abstraction(newBind, newBody.substitute(v, e))
    case Abstraction(bind, body) => Abstraction(bind, body.substitute(v, e, scope))
  }

  def headNormalForm: Lambda = {
    lazy val headNormalize: LL = memoize {
      case v@Variable(_) => v
      case Abstraction(bind, body) => Abstraction(bind, headNormalize(body))
      case Application(lhs, rhs) =>
       headNormalize(lhs) match {
          case Abstraction(bind, body) => body.substitute(bind, rhs)
          case hnf@_ => Application(hnf, rhs)
        }
    }

    headNormalize(this)
  }

  def normalForm: Lambda = {

    lazy val normalize: LL = memoize {
      case v@Variable(_) => v
      case Abstraction(bind, body) => Abstraction(bind, normalize(body))
      case Application(lhs, rhs) =>
        lhs.headNormalForm match {
          case Abstraction(bind, body) => body.substitute(bind, rhs).normalForm
          case hnf@_ => Application(hnf, normalize(rhs))
        }
    }

    normalize(this)
  }

  implicit class LambdaPrinting(val sc: StringContext) {
    def la(args: Lambda*): String = sc.s(args map parens: _*)
  }

  private def parens(expr: Lambda) = expr match {
    case Variable(name) => name
    case _ => "(" + expr.toString + ")"
  }
}

case class Abstraction(binding: Variable, body: Lambda) extends Lambda {
  override lazy val toString = la"λ$binding.$body"
}

case class Variable(name: String) extends Lambda {
  override lazy val toString = s"$name"

  def / : Variable = Variable(name + "'")
}

case class Application(lhs: Lambda, rhs: Lambda) extends Lambda {
  override lazy val toString = la"$lhs $rhs"
}

trait LambdaError

case class ParsingException(errorMessage: String = null) extends LambdaError {
  override def toString: String = s"Лямбда-выражение не соответствует грамматике: $errorMessage"
}

case class NoType() extends LambdaError {
  override lazy val toString: String = "Лямбда-выражение не имеет типа"
}
