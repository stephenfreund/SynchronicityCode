package anchor.compile

import anchor.lang._

class FindFieldWithCASOps() {

  private var fieldsWithCAS: Set[FieldDecl] = Set()

  def apply(x: Program): Set[FieldDecl] = {
    fieldsWithCAS = Set()
    x.classes.foreach(this (_))
    return fieldsWithCAS
  }

  private def apply(x: ClassDecl): Unit = {
    x.methods.foreach(this (_))
    this (x.constructor)
  }

  private def apply(x: MethodDecl): Unit = {
    this (x.stmt)
  }

  private def apply(x: ConstructorDecl): Unit = {
    this (x.stmt)
  }

  private def apply(x: Stmt): Unit = {
    x match {
      case VarDeclStmt(v, e)             => {
        e.foreach(apply(_))
      }
      case Assign(lhs, rhs)              => {
        apply(rhs)
      }
      case LocalAssign(assigns)          => {
        assigns.foreach(apply(_))
      }
      case Block(name, body)             => {
        for (s <- body) {
          this (s)
        }
      }
      case ExprStmt(i)                   => {
        this (i)
      }
      case Return(e, _)                  => {
        e.foreach(apply(_))
      }
      case SyncBlock(lock, stmt, _)      => {
        apply(lock)
        apply(stmt)
      }
      case If(cond, t, f)                => {
        apply(cond)
        apply(t)
        apply(f)
      }
      case While(cond, stmt, invs, decs) => {
        apply(cond)
        apply(stmt)
      }
      case Break(label)                  => {}

      case Yield(ensures)         =>
      case Commit()               =>
      case Assume(expr)           =>
      case Assert(expr)           =>
      case Invariant(expr)        =>
      case BoogieCode(_)          =>
      case NoReductionCheck(stmt) =>
        apply(stmt)
      case SyncStmt(op, expr)     => {
        apply(expr)
      }
    }
  }

  private def apply(x: Expr): Unit = {
    x match {
      case x@CAS(lhs, field, expected, rhs) => {
        apply(lhs)
        apply(expected)
        apply(rhs)
        this.fieldsWithCAS += x.decl
      }
      case ArrayAccess(a, i) => {
        this(a)
        this(i)
      }
      case FieldAccess(a, _, _) => {
        this(a)
      }
      case BinaryExpr(lhs, rhs, op)         => {
        apply(lhs)
        apply(rhs)
      }
      case UnaryExpr(expr, op)              => {
        apply(expr)
      }
      case Cond(p, tt, ff)                  => {
        apply(p)
        apply(tt)
        apply(ff)
      }
      case Invoke(ref, method, args, invs)  => {
        apply(ref)
        args.foreach(apply(_))
      }
      case x: PrimitiveFunction             =>
        apply(x)
      case Old(l)                           => apply(l)
      case _ =>
    }
  }

  private def apply(x: PrimitiveFunction): Unit = {
    x match {
      case Alloc(name, args, invs)   => {
        args.foreach(this (_))
      }
      case AAlloc(a, size)           => {
        apply(size)
      }
      case Length(a)                 => {
        apply(a)
      }
      case Lock(a)                   => {
        apply(a)
      }
      case IsLocal(expr, tid)        => {
        apply(expr)
        apply(tid)
      }
      case IsShared(expr)            => {
        apply(expr)
      }
      case IsFresh(e)             => apply(e);
      case Holds(expr, tid)          => {
        apply(expr)
        apply(tid)
      }
      case NextCASSucceeds(loc, tid) => {
        apply(loc)
        apply(tid)
      }
      case Rand()                    => {}
      case NextSpecStep(_)           => {}
    }
  }
}


