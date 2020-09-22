package anchor.sink

import anchor.tool.SinkPrinter

class Collection(val name: String,
                          val numParams : Int,
                          val functions : List[BuiltInFunctionDecl],
                          val boogieText : String) {
  def gen(sprinter : SinkPrinter, typeArgs : List[Type]): String = {
    var text = boogieText
    assert(numParams == typeArgs.size)
    for (i <- 0 until numParams) {
      text = text.replace(s"#${i}", sprinter.pp(typeArgs(i)))
      text = text.replace(s"#z${i}", sprinter.z3pp(typeArgs(i)))
      text = text.replace(s"#d${i}", sprinter.pp(sprinter.defaultValue(typeArgs(i))))
    }
    text
  }



}

class Library(val collections : Map[String, Collection]) {
  val functions = collections.flatMap(_._2.functions)

}



class GatherCollections {

  var collections = Set[CollectionType]()

  def apply(x: Program): Set[CollectionType] = {
    x.classes.foreach(c => this (c))
    x.globals.foreach(c => this (c))
    collections
  }

  def apply(x: ClassInvariant): Unit = {
    this(x.pred)
    x.triggers.foreach(_.foreach(this(_)))
  }

  def apply(x: ClassDecl): Unit = {
    x.arrays.foreach(this (_))
    x.fields.foreach(this (_))
    x.methods.foreach(this (_))
    x.invariants.foreach(this (_))
  }

  def apply(x: ArrayDecl): Unit = {
    this (x.elemType)
    this (x.spec)
  }

  def apply(x: FieldModifier): Unit = {
    x match {
      case VolatileModifier() =>
      case ABAFreeModifier()  =>
      case InternalModifier() =>
      case HasCASOperationModifier() =>
    }
  }


  def apply(x: FieldDecl): Unit = {
    this (x.t)
    this (x.spec)
    for (m <- x.modifiers) {
      this(m)
    }
  }


  def apply(x: Spec): Unit = {
    this (x.conditionalMover)
  }

  def apply(x: VarAccess): Unit = {
  }

  def apply(x: Location): Unit = {
    x match {
      case x@VarAccess(l)        => this (x)
      case FieldAccess(v, field) => this (v)
      case ArrayAccess(l, index) => this (l); this (index)
    }
  }


  def apply(x : Transaction): Unit = {
    x.modifies.foreach(this(_))
    x.ensures.foreach(this(_))
  }

  def apply(x : ExplicitMethodSpec): Unit = {
    x.requires.foreach(this(_))
      x.vars.foreach(this(_))
      x.transactions.foreach(this(_))
  }

  def apply(x: MethodDecl): Unit = {
    this (x.returnType)
    x.params.foreach(this (_))
    this(x.spec)
    this (x.stmt)
  }

  def apply(x: VarDecl): Unit = {
    this (x.t)
  }

  def apply(x: Stmt): Unit = {
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
      case Assume(x)                              => this (x)
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
  }

  def apply(x: Const): Unit = {
    x match {
      case IntConst(v)   =>
      case BoolConst(v)  =>
      case NullConst(t)  => this(t)
      case NullTid()     =>
      case MoverConst(m) =>
      case EmptyCollectionConst(t) => this(t)
    }
  }

  def apply(x: Expr): Unit = {
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
      case BuiltInFunctionCall(name, types, args) => types.foreach(this(_)); args.foreach(this(_))

    }
  }

  def apply(x : BinaryOp): Unit = {
  }

  def apply(x : UnaryOp): Unit = {
  }


  def apply(x: PrimitiveFunction): Unit = {
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
  }


  def apply(x: Type): Unit = {
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
      case x@CollectionType(kind, args) => {
        collections = collections + x
        args.foreach(this(_))

      }
    }
  }
}

object GatherCollections {
  def apply(p: Program): Set[CollectionType] = {
    new GatherCollections()(p)
  }
}