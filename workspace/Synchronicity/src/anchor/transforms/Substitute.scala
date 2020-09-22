package anchor.transforms

import anchor.sink.AST.pos
import anchor.sink._

/*
 * Only Suitable for using Houdini, because we get a Map of Strings, not VarDecls...
 */
case class Substitution(loc: Location, v: Expr)

class Substitute(val substs: List[Substitution]) {

  def eq(l1: Expr, l2: Expr): Boolean = {
    (l1, l2) match {
      case (x: VarAccess, y: VarAccess)     => x.decl eq y.decl
      case (x: FieldAccess, y: FieldAccess) => eq(x.l, y.l) && (x.decl eq y.decl)
      case (x: ArrayAccess, y: ArrayAccess) => eq(x.l, y.l) && (x.index == y.index)
      case _                                => false
    }
  }

  def apply(loc: Location): Expr = {
    val subst = substs.find(x => eq(x.loc, loc))
    pos(new DeepCopyWithPositions()(subst match {
      case Some(subst) => subst.v
      case None        => {
        loc match {
          case x@VarAccess(l)        => x
          case FieldAccess(v, field) => FieldAccess(this (v).asInstanceOf[Location], field)
          case ArrayAccess(l, index) => ArrayAccess(this (l).asInstanceOf[Location], this (index))
        }
      }
    }), loc.pos)
  }

  //  def apply(x: Location): Location = {
  //    pos(x match {
  //      case x@VarAccess(l)        => this(x).asInstanceOf[VarAccess]
  //      case FieldAccess(v, field) => FieldAccess(this (v), field)
  //      case ArrayAccess(l, index) => ArrayAccess(this (l), this (index))
  //      case Old(l)                => assert(false); Old(this (l)) // should not happen
  //    }, x.pos)
  //  }


  def apply(x: Const): Const = {
    pos(x match {
      case IntConst(v)   => IntConst(v)
      case BoolConst(v)  => BoolConst(v)
      case NullConst(t)  => NullConst(t)
      case NullTid()     => NullTid()
      case MoverConst(m) => MoverConst(m)
      case EmptyCollectionConst(t) => EmptyCollectionConst(t)
    }, x.pos)
  }

  def apply(x: Expr): Expr = {
    pos(x match {
      case ConstExpr(c)             => ConstExpr(this (c))
      case BinaryExpr(lhs, rhs, op) => BinaryExpr(this (lhs), this (rhs), DeepCopyWithPositions(op))
      case UnaryExpr(expr, op)      => UnaryExpr(this (expr), DeepCopyWithPositions(op))
      case Quantified(q, decls, pred, triggers)      => Quantified(q, decls.map(d => new DeepCopyWithPositions()(d)), this (pred), triggers.map(_.map(this(_))))
      case Cond(p, tt, ff)          => Cond(this (p), this (tt), this (ff))
      case p: Location              => this (p)
      case x: PrimitiveFunction     => this (x)
      case LoweredExpr(e, original) => LoweredExpr(this(e), this(original))
      case Old(l)                => Old(this(l))
      case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(DeepCopyWithPositions(_)), args.map(this(_)))
    }, x.pos)
  }

  private def apply(x: PrimitiveFunction): PrimitiveFunction = {
    pos(x match {
      case Length(a)             => Length(this (a).asInstanceOf[VarAccess])
      case Lock(a)             => Lock(this (a).asInstanceOf[VarAccess])
      case Holds(e, t)           => Holds(this (e).asInstanceOf[Location], this (t).asInstanceOf[VarAccess])
      case IsLocal(e, t)         => IsLocal(this (e).asInstanceOf[Location], this (t).asInstanceOf[VarAccess])
      case IsShared(e)           => IsShared(this (e).asInstanceOf[Location])
      case IsFresh(e)           => IsFresh(this (e).asInstanceOf[Location])
      case MoverPermission(e, v) => MoverPermission(this (e).asInstanceOf[Location], v.map(apply))
      case NextSpecStep(n) => NextSpecStep(n)
      case GoesWrong(e) => GoesWrong(this(e))
      case Rand()                => Rand()
    }, x.pos)
  }
}

object Substitute {
  def apply(x : Expr, l : Location, e: Expr) = {
    new Substitute(List(Substitution(l,e)))(x)
  }
}