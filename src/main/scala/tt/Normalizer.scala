package tt

import scala.collection.mutable

class Normalizer {

  var count = 0

  def nextFreeName() = {
    count += 1
    s"v_$count"
  }

  case class Redex(v: String, body: Lambda, arg: Lambda)

  val cache = mutable.WeakHashMap[Redex, Lambda]()
  val vars = mutable.Map[String, Variable]()

  def substitute(what: Lambda, varName: String, e: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v@Variable(name) => if (name == varName) e else v
    case Application(x, y) => Application(substitute(x, varName, e, scope), substitute(y, varName, e, scope))
    case t@Abstraction(Variable(name), body) =>
      val bodyFree = body.freeVariables().map(_.name)
      if (name == varName || !bodyFree.contains(varName)) {
        t
      } else {
        val free = nextFreeName()
        val freeVar = vars.getOrElseUpdate(free, Variable(free))
        val subst = substitute(substitute(body, name, freeVar, scope + free), varName, e, scope + free)
        Abstraction(freeVar, subst)
      }
  }

  def headNormalForm(what: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v: Variable => v
    case Abstraction(bind, body) => Abstraction(bind, headNormalForm(body, scope + bind.name))
    case Application(lhs, rhs) =>
      headNormalForm(lhs, scope) match {
        case Abstraction(v, body) =>
          val redex = Redex(v.name, body, rhs)
          cache.getOrElseUpdate(redex,
            headNormalForm(substitute(body, v.name, rhs, scope + v.name), scope + v.name))
        case o => Application(o, rhs)
      }
  }

  def normalForm(what: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v: Variable => v
    case Abstraction(v, body) => Abstraction(v, normalForm(body, scope + v.name))
    case Application(lhs, rhs) =>
      headNormalForm(lhs) match {
        case Abstraction(v, body) => normalForm(substitute(body, v.name, rhs, scope + v.name), scope + v.name)
        case o => Application(normalForm(o), normalForm(rhs))
      }
  }
}

object Normalizer {
  def apply(l: Lambda): Lambda = new Normalizer().normalForm(l)
}
