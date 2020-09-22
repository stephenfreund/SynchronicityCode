package anchor.transforms

import anchor.sink.AST.pos
import anchor.sink._

import scala.util.parsing.input.Position

class FixPosition(val fixedPosition : Position) {

  def apply(x: Program): Program = {
    pos(x, fixedPosition)
    x.classes.foreach(c => this (c))
    x.globals.foreach(c => this (c))
    x
  }

  def apply(x: ClassInvariant) : ClassInvariant = {
    pos(x, fixedPosition)
    this(x.pred)
    x.triggers.foreach(_.foreach(this(_)))
    x
  }

  def apply(x: ClassDecl): ClassDecl = {
    pos(x, fixedPosition)
    x.arrays.foreach(this (_))
    x.fields.foreach(this (_))
    x.methods.foreach(this (_))
    x.invariants.foreach(this (_))
    x
  }

  def apply(x: ArrayDecl): ArrayDecl = {
    pos(x, fixedPosition)
    this (x.elemType)
    this (x.spec)
    x
  }

  def apply(x: FieldModifier) : Unit = {
    pos(x, fixedPosition)
    x match {
      case VolatileModifier() =>
      case ABAFreeModifier()  =>
      case InternalModifier() =>
      case HasCASOperationModifier() =>
    }
  }


  def apply(x: FieldDecl): FieldDecl = {
    pos(x, fixedPosition)
    this (x.t)
    this (x.spec)
    for (m <- x.modifiers) {
      this(m)
    }
    x
  }


  def apply(x: Spec): Spec = {
    pos(x, fixedPosition)
    this (x.conditionalMover)
    x
  }

  def apply(x: VarAccess): VarAccess = {
    pos(x, fixedPosition)
    x
  }

  def apply(x: Location): Location = {
    pos(x, fixedPosition)
    x match {
      case x@VarAccess(l)        => this (x)
      case FieldAccess(v, field) => this (v)
      case ArrayAccess(l, index) => this (l); this (index)
    }
    x
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
    pos(x, fixedPosition)
    this (x.returnType)
    x.params.foreach(this (_))
    this(x.spec)
    this (x.stmt)
    x
  }

  def apply(x: VarDecl): VarDecl = {
    pos(x, fixedPosition)
    this (x.t)
    x
  }

  def apply(x: Stmt): Stmt = {
    pos(x, fixedPosition)
    x match {
      case VarDeclStmt(v)                         => this (v)
      case Assign(lhs, rhs)                       => lhs.foreach(this (_)); rhs.foreach(this (_))
      case Block(n, body)                => body.foreach(this (_))
      case Write(lhs, field, rhs, _)                 => this (lhs); this (rhs)
      case LocalWrites(writes)                    => writes.foreach(this (_))
      case Read(lhs, rhs, field, _)                  => this (lhs); this (rhs)
      case Invoke(ref, method, args, res, invs)   => this (ref); args.foreach(this (_)); res.foreach(this (_)); invs.foreach(this (_))
      case InlineInvoke(invoke) => this (invoke)
      case InlineReturn() =>
      case Return(e, isSynthetic) => e.foreach(this (_))
      case Sync(lock, stmt, pos) => this (lock); this (stmt)
      case If(cond, t, f) => this (cond); this (t); this (f)
      case While(cond, stmt, invs, decreases)     => this (cond); this (stmt); invs.foreach(this (_)); decreases.foreach(this (_))
      case Break(l)                               =>
      case CAS(result, lhs, field, expected, rhs) => this (result); this (lhs); this (expected); this (rhs)
      case Alloc(lhs, name) => this (lhs);
      case Yield(ensures) => ensures.foreach(this (_))
      case Commit() =>
      case Assume(x)                              => Assume(this (x))
      case Assert(x)                              => this (x)
      case Invariant(x) => this (x)
      case AWrite(lhs, i, rhs) => this (lhs); this (i); this (rhs)
      case ARead(lhs, rhs, i)                     => this (lhs); this (rhs); this (i)
      case AAlloc(lhs, a, size)                   => this (lhs); this (a); this (size)
      case BoogieCode(s)                          =>
      case NoReductionCheck(s)                    => this (s)
      case Acquire(x)                             => this (x)
      case Release(x)                             => this (x)
    }
    x
  }

  def apply(x: Const): Const = {
    pos(x, fixedPosition)
      x match {
        case IntConst(v)   =>
        case BoolConst(v)  =>
        case NullConst(t)  => this(t)
        case NullTid()     =>
        case MoverConst(m) =>
        case EmptyCollectionConst(t) => this(t)
      }
    x
  }

  def apply(x: Expr): Expr = {
    pos(x, fixedPosition)
    x match {
      case ConstExpr(c)             => this (c)
      case BinaryExpr(lhs, rhs, op) => this (lhs); this (rhs); this(op)
      case UnaryExpr(expr, op)      => this (expr); this(op)
      case Quantified(q, decls, pred, triggers)      => decls.foreach(this (_)); this (pred); triggers.foreach(_.foreach(this(_)))
      case Cond(p, tt, ff)          => this (p); this (tt); this (ff)
      case p: Location              => this (p)
      case x: PrimitiveFunction     => this (x)
      case LoweredExpr(e, original) => this(e); this(original)
      case Old(e) => this(e)
      case BuiltInFunctionCall(name, types, args) => types.map(this(_)); args.map(this(_))

    }
    x
  }

  def apply(x : BinaryOp) = {
    pos(x, fixedPosition)
  }

  def apply(x : UnaryOp) = {
    pos(x, fixedPosition)
  }


  def apply(x: PrimitiveFunction): PrimitiveFunction = {
    pos(x, fixedPosition)
    x match {
      case Length(a)             => this (a)
      case Lock(a)             => this (a)
      case Holds(e, t)           => this (e); this (t)
      case IsLocal(e, t)         => this (e); this (t)
      case IsShared(e)           => this (e)
      case IsFresh(e)           => this (e)
      case MoverPermission(e, v) => this (e); v.foreach(apply)
      case GoesWrong(e) => this(e)
      case Rand()                =>
      case NextSpecStep(n) =>
    }
    x
  }


  def apply(x: Type): Type = {
    pos(x, fixedPosition)
    x match {
      case IntType()                            =>
      case TidType()                            =>
      case VoidType()                           =>
      case BoolType()                           =>
      case MoverType()                          =>
      case BoogieType(name)                     =>
      case ClassType(ident)                     =>
      case ArrayType(enclosing, ident, thisVar) => this (thisVar)
      case TypeVar(x) =>
      case CollectionType(kind, args) => args.foreach(this(_))
    }
    x
  }
}

object FixPosition {
  def apply(p: Position) = {
    new FixPosition(p)
  }
}