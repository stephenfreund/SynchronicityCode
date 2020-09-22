package anchor.transforms

import anchor.sink.AST.pos
import anchor.sink._

/*
 * Only Suitable for using Houdini, because we get a Map of Strings, not VarDecls...
 */
class SubstBoolValues(val substs: Map[String, Boolean], val substOnlyFalseVars : Boolean) {


  def apply(x: Program): Program = {
    val globals = if (substOnlyFalseVars) {
      x.globals.filter(v => substs.getOrElse(v.name, true)).map(c => this(c))
    } else {
      x.globals.filter(v => !substs.contains(v.name)).map(c => this(c))
    }
    pos(Program(x.classes.map(c => this(c)), globals,  x.axioms.map(this(_)), x.library), x.pos)
  }

  def dropTruesFromClassInvariants(exprs : List[ClassInvariant]) : List[ClassInvariant] = {
    exprs.filter(_.pred != ConstExpr(BoolConst(true)))
  }

  def dropTrues(exprs : List[Expr]) : List[Expr] = {
    exprs.filter(_ != ConstExpr(BoolConst(true)))
  }

  def apply(x : ClassInvariant): ClassInvariant  = {
    pos(ClassInvariant(this(x.pred), x.triggers.map(_.map(this(_)))), x.pos)
  }

  def apply(x: ClassDecl): ClassDecl = {
    pos(ClassDecl(x.name, x.arrays.map(this(_)), x.fields.map(this(_)), x.methods.map(this(_)), dropTruesFromClassInvariants(x.invariants.map(this(_)))), x.pos)
  }

  def apply(x: ArrayDecl): ArrayDecl = {
    pos(ArrayDecl(x.name, this(x.elemType), x.elemName, this(x.spec)), x.pos)
  }

  def apply(x : FieldModifier) : FieldModifier = {
    pos(x match {
      case VolatileModifier() => VolatileModifier()
      case ABAFreeModifier()  => ABAFreeModifier()
      case InternalModifier() => InternalModifier()
      case HasCASOperationModifier() => HasCASOperationModifier()
    }, x.pos)
  }

  def apply(x: FieldDecl): FieldDecl = {
    pos(FieldDecl(this(x.t), x.name, this(x.spec), x.modifiers.map(this(_))), x.pos)
  }


  private def apply(x : Spec): Spec = {
    pos(Spec(this(x.conditionalMover), x.blocking, x.yieldsAs.map(this(_))), x.pos)
  }

  def apply(x: VarAccess): VarAccess = { pos(VarAccess(x.name), x.pos) }

  def apply(x: Location): Location = {
    pos(x match {
      case x@VarAccess(l)          => this(x)
      case FieldAccess(v, field) => FieldAccess(this (v), field)
      case ArrayAccess(l, index) => ArrayAccess(this (l), this (index))
    }, x.pos)
  }


  def apply(x : Transaction): Transaction = {
    pos(Transaction(x.repeats, x.modifies.map(this(_)), x.ensures.map(this(_))), x.pos)
  }

  def apply(x : ExplicitMethodSpec) : ExplicitMethodSpec = {
    pos(ExplicitMethodSpec(x.requires.map(this(_)),
      x.vars.map(this(_)),
      x.transactions.map(this(_))), x.pos)
  }


  def apply(x: MethodDecl): MethodDecl = {
    pos(MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), this(x.spec), this (x.stmt)),
      x.pos)
  }

  def apply(x: VarDecl): VarDecl = {
    pos(VarDecl(this(x.t), x.name), x.pos)
  }

  def apply(x: Stmt): Stmt = {
    pos(x match {
      case VarDeclStmt(v)                         => VarDeclStmt(this (v))
      case Assign(lhs, rhs)                       => Assign(lhs.map(this (_)), rhs.map(this (_)))
      case Block(n, body)                => Block(n, body.map(this (_)))
      case Write(lhs, field, rhs, movesAs)                 => Write(this (lhs), field, this (rhs), movesAs)
      case LocalWrites(writes)                    => LocalWrites(writes.map(this (_).asInstanceOf[Write]))
      case Read(lhs, rhs, field, movesAs)                  => Read(this (lhs), this (rhs), field, movesAs)
      case Invoke(ref, method, args, res, invs)   => Invoke(this (ref), method, args.map(this (_)), res.map(this (_)), invs.map(this (_)))
      case InlineInvoke(invoke) => InlineInvoke(this (invoke).asInstanceOf[Invoke])
      case InlineReturn() => InlineReturn()
      case Return(e, isSynthetic)                 => Return(e.map(this (_)), isSynthetic)
      case Sync(lock, stmt, pos)                  => Sync(this (lock), this (stmt), pos)
      case If(cond, t, f)                         => If(this (cond), this (t), this (f))
      case While(cond, stmt, invs, decreases)     => While(this (cond), this (stmt), dropTrues(invs.map(this (_))), decreases.map(this (_)))
      case Break(l) => Break(l)
      case CAS(result, lhs, field, expected, rhs) => CAS(this (result), this (lhs), field, this (expected), this (rhs))
      case Alloc(lhs, name) => Alloc(this (lhs), name)
      case Yield(ensures) => Yield(ensures.map(this (_)))
      case Commit() => Commit()
      case Assume(x) => Assume(this (x))
      case Assert(x)                              => Assert(this (x))
      case Invariant(x)                           => Invariant(this (x))
      case AWrite(lhs, i, rhs) => AWrite(this (lhs), this (i), this (rhs))
      case ARead(lhs, rhs, i) => ARead(this (lhs), this (rhs), this (i))
      case AAlloc(lhs, a, size)                   => AAlloc(this (lhs), this (a).asInstanceOf[ArrayType], this (size))
      case BoogieCode(s)                          => BoogieCode(s)
      case NoReductionCheck(s)                    => NoReductionCheck(this (s))
      case Acquire(x)                             => Acquire(this (x))
      case Release(x)                             => Release(this (x))
    }, x.pos)
  }

  def apply(x: VarOrConst) : VarOrConst = {
    x match {
      case y : ConstExpr => apply(y)
      case y : VarAccess => apply(y)
    }
  }

  def apply(x : ConstExpr) : ConstExpr = {
    pos(ConstExpr(this (x.const)), x.pos)
  }

  def apply(x: Const): Const = {
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
    pos(x match {
      case ConstExpr(c)                                                      => ConstExpr(this (c))
      case BinaryExpr(lhs, rhs, op)                                          => AST.binary(this (lhs), this (rhs), DeepCopyWithPositions(op))
      case UnaryExpr(expr, op)                                               => AST.unary(this (expr), DeepCopyWithPositions(op))
      case Quantified(q, decls, pred, triggers)                                     => Quantified(q, decls.map(this (_)), this (pred), triggers.map(_.map(this (_))))
      case Cond(p, tt, ff)                                                   => Cond(this (p), this (tt), this (ff))
      case x@VarAccess(name) if x.scope.isGlobal(x) && substs.contains(name) => {
        val value = substs(name)
        if (!substOnlyFalseVars || !value) {
          ConstExpr(pos(BoolConst(substs(name)), x))
        } else {
          VarAccess(name)
        }
      }
      case p: Location                                                       => this (p)
      case x: PrimitiveFunction                                              => this (x)
      case LoweredExpr(e, original)                                          => LoweredExpr(this (e), this (original))
      case Old(l)                => Old(this (l))
    }, x.pos)
  }

  private def apply(x: PrimitiveFunction): PrimitiveFunction = {
    pos(x match {
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
    }, x.pos)
  }


  def apply(x: Type): Type = {
    pos(x match {
      case IntType() => IntType()
      case TidType() => TidType()
      case VoidType() => VoidType()
      case BoolType() => BoolType()
      case MoverType() => MoverType()
      case BoogieType(name) => BoogieType(name)
      case ClassType(ident) => ClassType(ident)
      case ArrayType(enclosing, ident, thisVar) =>
        ArrayType(enclosing, ident, this(thisVar))
      case TypeVar(x) => TypeVar(x)
      case CollectionType(kind, typeArgs) => CollectionType(kind, typeArgs.map(this(_)))
    }, x.pos)
  }
}
