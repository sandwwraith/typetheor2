package tt

import tt.TypeOpts._

class Typer {

  private val freeTypes: Iterator[Atom] = Stream.from(0).map(i => Atom(s"a$i")).iterator

  def buildSystem(expr: Lambda, ctx: Map[Variable, Type]): (Seq[Equation], Type) = expr match {
    case v@Variable(_) => (Seq(), ctx.getOrElse(v, freeTypes.next))
    case Application(fun, arg) =>
      val (sysF, typeF) = buildSystem(fun, ctx)
      val (sysA, typeA) = buildSystem(arg, ctx)
      val retType = freeTypes.next
      (sysF ++ sysA :+ Equation(typeF, typeA -> retType), retType)
    case Abstraction(v, body) =>
      val (_, typeV) = buildSystem(v, ctx)
      val (sysB, typeB) = buildSystem(body, ctx + (v -> typeV))
      (sysB, typeV -> typeB)
  }

  def unify(system: Seq[Equation]): Option[Seq[Equation]] = {
    val allVars = system.flatMap(_.vars).toSet
    val res = unify_(system)
    res match {
      case Some(list) =>
        val left = list.map(_.lhs).toSet
        val more = allVars.filter(!left.contains(_)).map(p => Equation(p, p)).toSeq
        Some(more ++ list)
      case None => None
    }
  }


  /**
    * http://www.nsl.com/misc/papers/martelli-montanari.pdf
    */
  private def unify_(system: Seq[Equation]): Option[Seq[Equation]] = system match {
    case Nil => Some(Seq())
    case eqs if eqs.exists { eq => eq.isSame } => unify_(eqs.flatMap(_.unifyArgs))
    case eqs if eqs.exists { eq => eq.isDiff } => None
    case eqs if eqs.exists { eq => eq.lhs == eq.rhs } => unify_(eqs.filter { eq => eq.lhs != eq.rhs })
    case eqs if eqs.exists { term => term.swap != term } => unify_(eqs.map(_.swap))
    case eqs if eqs.exists {
      case eq@Equation(v@Var(_), Fun(_, args)) => args.exists {
        _.contains(v)
      }
      case _ => false
    } => None
    case eqs =>
      val tmp = eqs.find {
        case eq@Equation(v@Var(_), t) => v != t && eqs.filter(_ != eq).exists {
          _.contains(v)
        }
        case _ => false
      }
      tmp match {
        case Some(e@Equation(v@Var(_), t)) => unify_(Equation(v, t) +: eqs.filter(_ != e).map(_.substitute(v, t)))
        case _ => Some(eqs)
      }
  }
}

object Typer {
  private def substituteTypes(where: Type, what: Map[Type, Type]): Type = where match {
    case t@Atom(_) => what.getOrElse(t, t)
    case Arrow(left, right) => substituteTypes(left, what) -> substituteTypes(right, what)
  }

  def apply(l: Lambda): Option[Type] = {
    val typer = new Typer()
    val (system, newtype) = typer.buildSystem(l, Map.empty)
    typer.unify(system).map { answer =>
      val map = answer.map { eq => termToType(eq.lhs) → termToType(eq.rhs) }.toMap
      substituteTypes(newtype, map)
    }
  }
}
