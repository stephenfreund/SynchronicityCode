package anchor.transforms

import anchor.sink._
import PrettyPrint._

class Rename(val names : Map[VarDecl, VarDecl]) {

  var currentMethod: MethodDecl = null

  private def apply(x: VarAccess): VarAccess = {
    assert (x.decl != null, s"${x} has no decl.")
    if (names.contains(x.decl)) {
      val decl = names(x.decl)
      new VarAccess(decl)
    } else {
      x
    }
  }

  def apply(x : Spec): Spec = {
    Spec(this(x.conditionalMover), x.blocking, x.yieldsAs.map(this(_)))
  }

  private def apply(x : Mover): Mover = {
    x match {
      case I() => I()
      case B() => B()
      case R() => R()
      case L() => L()
      case N() => N()
      case E() => E()
    }
  }

  def apply(x: Location): Location = {
    x match {
      case x @ VarAccess(v) => {
        val r = this(x)
        r.t = this(x.t)
        r.decl = x.decl
        r
      }
      case x @ FieldAccess(v, field) => {
        val r = FieldAccess(this(v), (field))
        r.decl = x.decl
        r.t = this(x.t)
        r
      }
      case x @ ArrayAccess(l, index) => {
        val r = ArrayAccess(this(l), this(index))
        r.t = this(x.t)
        r
      }
    }
  }

  private def apply(x: Const): Const = {
    x match {
      case IntConst(v)   => IntConst(v)
      case BoolConst(v)  => BoolConst(v)
      case NullConst(t)  => NullConst(this(t).asInstanceOf[RefType])
      case NullTid()     => NullTid()
      case MoverConst(m) => MoverConst(m)
      case EmptyCollectionConst(t) => EmptyCollectionConst(this(t).asInstanceOf[CollectionType])
    }
  }

  def apply(x: Expr): Expr = {

    val r = x match {
      case ConstExpr(c) => {
        ConstExpr(this(c))
      }
      case BinaryExpr(lhs, rhs, op) => {
        BinaryExpr(this(lhs), this(rhs), DeepCopyWithPositions(op))
      }
      case UnaryExpr(expr, op) => {
        UnaryExpr(this(expr), DeepCopyWithPositions(op))
      }
      case Quantified(q, decls,pred, triggers)       => {
        Quantified(q, decls, this(pred), triggers.map(_.map(this(_))))
      }
      case Cond(p, tt, ff) => {
        Cond(this(p), this(tt), this(ff))
      }
      case x: PrimitiveFunction => {
        this(x)
      }
      case x: Location => {
        this(x)
      }
      case LoweredExpr(e, original) => LoweredExpr(this(e), this(original))
      case x @ Old(l) => Old(this(l))
      case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(this(_)), args.map(this(_)))
    }
    r.t = this(x.t)
    r
  }

  private def apply(x: PrimitiveFunction): PrimitiveFunction = {
    x match {
      case Length(a)             => Length(this (a))
      case Lock(a)             => Lock(this (a))
      case Holds(e, t)           => Holds(this (e), this (t))
      case IsLocal(e, t)         => IsLocal(this (e), this (t))
      case IsShared(e)           => IsShared(this (e))
      case IsFresh(e)           => IsFresh(this (e))
      case MoverPermission(e, v) => MoverPermission(this (e), v.map(apply))
      case GoesWrong(e) => GoesWrong(this(e))
      case Rand()                => Rand()
      case NextSpecStep(n) => NextSpecStep(n)
    }
  }

  def apply(x: Type): Type = {
    x match {
      case x@ClassType(c) => {
        val r = ClassType(c)
        r.decl = x.decl
        r
      }
      case x@ArrayType(enclosing, ident, thisVar) => {
        val r = ArrayType(enclosing, ident, this(thisVar))
        r.decl = x.decl
        r
      }
      case x => x
    }
  }
}

object Rename {

  def apply(x: Spec, from: VarDecl, to:VarDecl) : Spec = {
    apply(x, Map[VarDecl,VarDecl](from->to))
  }

  def apply(x: Spec, map: Map[VarDecl,VarDecl]) : Spec = {
    val r = new Rename(map)
    r(x)
  }

  def apply(x: Expr, from: VarDecl, to:VarDecl): Expr = {
    apply(x, Map[VarDecl,VarDecl](from->to))
  }

  def apply(x: Expr, map: Map[VarDecl,VarDecl]): Expr = {
    val r = new Rename(map)
    r(x)
  }

}
