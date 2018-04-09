package tt

sealed abstract class Type {
  def ->(other: Type): Type = Arrow(this, other)

  def freeTypes(): Set[String] = this match {
    case Atom(s) => Set(s)
    case Arrow(f, t) => f.freeTypes() ++ t.freeTypes()
    case ForAll(name, t) => t.freeTypes() - name
  }
}

case class Atom(name: String) extends Type {
  override lazy val toString: String = name
}

case class Arrow(from: Type, to: Type) extends Type {
  override def toString: String = (from, to) match {
    case (a@Atom(_), b) => s"$a -> $b"
    case (a@_, b) => s"($a) -> $b"
  }
}

case class ForAll(v: String, t: Type) extends Type {
  override lazy val toString: String = s"∀$v($t)"
}

sealed abstract class LTerm {
  def vars: Set[Var]

  def contains(v: Var): Boolean = vars.contains(v)

  def substitute(from: Var, to: LTerm): LTerm = this match {
    case Var(_) if this == from => to
    case Fun(name, args) => Fun(name, args.map(_.substitute(from, to)))
    case _ => this
  }
}

case class Var(name: String) extends LTerm {
  override def vars: Set[Var] = Set(this)

  override lazy val toString: String = name
}

case class Fun(name: String, args: Seq[LTerm]) extends LTerm {
  override def vars: Set[Var] = args.flatMap(_.vars).toSet

  override lazy val toString: String = s"$name(${args.mkString(", ")})"
}

case class Equation(lhs: LTerm, rhs: LTerm) extends LTerm {
  override def vars: Set[Var] = lhs.vars ++ rhs.vars

  override lazy val toString: String = s"$lhs = $rhs"

  def swap: Equation = this match {
    case Equation(f @ Fun(_, _), v @ Var(_)) => Equation(v, f)
    case _ => this
  }

  def isSame: Boolean = this match {
    case Equation(f @ Fun(_, _), g @ Fun(_, _)) => f.name == g.name && f.args.length == g.args.length
    case _ => false
  }

  def isDiff: Boolean = this match {
    case Equation(f @ Fun(_, _), g @ Fun(_, _)) => f.name != g.name || f.args.length != g.args.length
    case _ => false
  }

  def unifyArgs: Seq[Equation] = this match {
    case Equation(f @ Fun(_, _), g @ Fun(_, _)) if isSame => (f.args, g.args).zipped.map(Equation)
    case _ => Seq(this)
  }

  override def substitute(from: Var, to: LTerm): Equation = Equation(lhs.substitute(from, to), rhs.substitute(from, to))
}

object TypeOpts {
  implicit def typeToTerm(t: Type): LTerm = t match {
    case Atom(name) => Var(name)
    case Arrow(from, to) => Fun("->", Seq(typeToTerm(from), typeToTerm(to)))
    case ForAll(v, tt) => Fun("forall", Seq(typeToTerm(Var(v)), typeToTerm(tt)))
  }

  implicit def termToType(t: LTerm): Type = t match {
    case Var(name) => Atom(name)
    case Fun("->", Seq(from, to)) => Arrow(termToType(from), termToType(to))
    case Fun("forall", Seq(Var(x), to)) => ForAll(x, termToType(to))
  }
}


