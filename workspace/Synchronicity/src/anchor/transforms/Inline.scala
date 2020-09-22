package anchor.transforms

import anchor.sink._
import PrettyPrint._
import anchor.util.Errors._

import scala.util.parsing.input.{Position, Positional}

object Inline {
  var count: Int = 0
  def unique(): Int = {
    count += 1
    count
  }

  def apply(x: Program): Program = {
    val p = Program(x.classes.map(this(_)), x.globals, x.axioms, x.library)
    // must re-typecheck...
    BuildScope.annotate(p)
    TypeCheck.tc(p)
    p
  }

  private def apply(x: ClassDecl): ClassDecl = {
    AST.pos(ClassDecl(x.name, x.arrays, x.fields, x.methods.map(this(_)), x.invariants), x)
  }

  private def apply(x: MethodDecl): MethodDecl = {
    if (x.isPublic) {
      AST.pos(MethodDecl(x.isPublic, x.returnType, x.name, x.params, x.spec, this (x.stmt).asInstanceOf[Block]), x)
    } else {
      DeepCopyWithPositions(x)
    }
  }

  private def apply(x: Stmt): Stmt = {
    val xx = x match {
      case Block(label, body) => {
        Block(label, body.map(this (_)))
      }
      case x@Invoke(ref, method, args, res, invariants) => {
        (new Inline(x.pos, DeepCopyWithPositions(invariants))) (x)
      }
      case Sync(lock, stmt, pos) => {
        Sync(lock, this (stmt).asInstanceOf[Block], pos)
      }
      case If(cond, t, f) => {
        If(cond, this (t).asInstanceOf[Block], this (f).asInstanceOf[Block])
      }
      case While(cond, stmt, invs, decreases) => {
        While(cond, this (stmt).asInstanceOf[Block], invs, decreases)
      }
      case x => DeepCopyWithPositions(x)
    }
    AST.pos(xx, x)
  }
}

class InlinedPosition(val pos: Position, val inlinePos: Position) extends Position {
  def line : scala.Int = pos.line
  def column : scala.Int = pos.column
  override def lineContents : String = ""
  override def toString() : java.lang.String = {
    pos.toString()
  }
  override def longString : java.lang.String = {
    pos.longString + s"\n  inlined at (${inlinePos}):\n${inlinePos.longString}"
  }
  override def <(that : scala.util.parsing.input.Position) : scala.Boolean = pos < that
}


private class Inline(val inlinePos: Position, val enclosingInvariants : List[Expr], val suffix: String = s"${Inline.unique}") {

  def pos[T <: Positional](t : T, pos: Position) : T = {
    t.pos = new InlinedPosition(pos, inlinePos)
    t
  }

  def pos[T <: Positional](t : T, u : Positional) : T = {
    pos(t, u.pos)
  }


  def apply(invoke: Invoke): Block = {
    val label = remap(invoke, "exit")
    val argParams =
      invoke.args.zip(invoke.decl.params)
    val decls : List[Stmt] = invoke.decl.params.map(p => pos(VarDeclStmt(remap(invoke, p)),invoke)) :+
                  pos(VarDeclStmt(pos(remap(invoke, pos(VarDecl(pos(ClassType(invoke.decl.parent.name), invoke), "this"), invoke)), invoke)), invoke)
    val assigns : List[Stmt] = argParams.map(p => pos(Assign(pos(VarAccess(remap(invoke, p._2.name)), invoke), p._1), invoke)) :+
                  pos(Assign(pos(VarAccess(remap(invoke, "this")), invoke), invoke.ref), invoke)
    val stmts : Stmt = remap(invoke, invoke.decl.stmt)
    Block(None,
      List(pos(InlineInvoke(new DeepCopyWithPositions()(invoke).asInstanceOf[Invoke]), invoke.pos),
      pos(Block(Some(label), decls ++ assigns ++ List(stmts)), invoke),
      pos(InlineReturn(), invoke.pos)))
  }

  private def remap(invoke: Invoke, x: String): String = {
    if (x == "tid") {
      "tid"
    } else {
      s"${x}_${suffix}"
    }
  }

  private def remap(invoke: Invoke, x: VarDecl): VarDecl = {
    pos(VarDecl(remap(invoke, x.t), remap(invoke, x.name)), x)
  }

  private def remap(invoke: Invoke, x: Stmt): Stmt = {
    pos(x match {
      case VarDeclStmt(v)                               => {
        VarDeclStmt(remap(invoke, v))
      }
      case Assign(lhs, rhs)                             => {
        Assign(lhs.map(remap(invoke, _)), rhs.map(remap(invoke, _)))
      }
      case Block(label, body)                  => {
        Block(label.map(remap(invoke, _)), body.map(remap(invoke, _)))
      }
      case x@Write(lhs, field, rhs, movesAs)                     => {
        Write(remap(invoke, lhs), field, remap(invoke, rhs), movesAs)
      }
      case LocalWrites(writes)                          => {
        LocalWrites(writes.map(remap(invoke, _).asInstanceOf[Write]))
      }
      case x@Read(lhs, rhs, field, movesAs)                      => {
        Read(remap(invoke, lhs), remap(invoke, rhs), field, movesAs)
      }
      case x@Invoke(ref, method, args, res, invariants) => {
        val y = pos(Invoke(remap(invoke, ref), method, args.map(remap(invoke, _)), res.map(remap(invoke, _)), invariants.map(remap(invoke, _))), x)
        y.decl = x.decl
        assert(y.decl != null);
        (new Inline(new InlinedPosition(x.pos, inlinePos), enclosingInvariants ++ invariants, s"${suffix}_${Inline.unique}")) (y)
      }
      case InlineInvoke(_) => fail("Inline", "Should not see inlined invoke before inlining.")
      case InlineReturn() => fail("Inline", "Should not see inlined invoke before inlining.")
      case Return(None, _)                              => {
        pos(Break(Some(remap(invoke, "exit"))), x)
      }

      case Return(Some(e), _)                     => {
        invoke.result match {
          case Some(r) => Block(None, List(pos(Assign(r, remap(invoke, e)), x), pos(Break(Some(remap(invoke, "exit"))), x)))
          case None    => Block(None, List(pos(Break(Some(remap(invoke, "exit"))), x)))
        }

      }
      case Sync(lock, stmt, pos)                  => {
        Sync(remap(invoke, lock), remap(invoke, stmt).asInstanceOf[Block], pos)
      }
      case If(cond, t, f)                         => {
        If(remap(invoke, cond), remap(invoke, t).asInstanceOf[Block], remap(invoke, f).asInstanceOf[Block])
      }
      case While(cond, stmt, invs, decreases)     => {
        While(remap(invoke, cond), remap(invoke, stmt).asInstanceOf[Block], invs.map(remap(invoke, _)) ++ enclosingInvariants, decreases.map(remap(invoke, _)))
      }
      case Break(label)                           => Break(label.map(remap(invoke, _)))
      case CAS(result, lhs, field, expected, rhs) => {
        CAS(remap(invoke, result), remap(invoke, lhs), field, remap(invoke, expected), remap(invoke, rhs))
      }
      case Alloc(lhs, t)                          => {
        Alloc(remap(invoke, lhs), t)
      }
      case Yield(ensures)                         => Yield(ensures.map(remap(invoke, _)))
      case Commit()                               => Commit()
      case Assume(x)                              => Assume(remap(invoke, x))
      case Assert(x)                              => Assert(remap(invoke, x))
      case Invariant(x)                           => Invariant(remap(invoke, x))
      case x@AWrite(lhs, i, rhs)                  => {
        AWrite(remap(invoke, lhs), remap(invoke, i), remap(invoke, rhs))
      }
      case x@ARead(lhs, rhs, i)                   => {
        ARead(remap(invoke, lhs), remap(invoke, rhs), remap(invoke, i))
      }
      case AAlloc(lhs, t, i)                      => {
        AAlloc(remap(invoke, lhs), remap(invoke, t).asInstanceOf[ArrayType], remap(invoke, i))
      }


      case BoogieCode(x)       => BoogieCode(x)
      case NoReductionCheck(s) => NoReductionCheck(remap(invoke, s))
      case Acquire(x)          => Acquire(remap(invoke, x))
      case Release(x)          => Release(remap(invoke, x))
    }, x)
  }

  def remap(invoke : Invoke, x: VarOrConst) : VarOrConst = {
    x match {
      case y : ConstExpr => remap(invoke, y)
      case y : VarAccess => remap(invoke, y)
    }
  }

  def remap(invoke: Invoke, x : ConstExpr) : ConstExpr = {
    pos(ConstExpr(remap(invoke, x.const)), x.pos)
  }

  def remap(invoke: Invoke, x: VarAccess): VarAccess = {
    if (x.scope.isGlobal(x)) {
      pos(VarAccess(x.name), x)
    } else {
      pos(VarAccess(remap(invoke, x.name)), x)
    }
  }

  private def remap(invoke: Invoke, x: Location): Location = {
    pos(x match {
      case x@VarAccess(v)          => {
        remap(invoke, x)
      }
      case FieldAccess(p, field) => {
        FieldAccess(remap(invoke, p), field)
      }
      case ArrayAccess(l, index) => {
        ArrayAccess(remap(invoke, l), remap(invoke, index))
      }
    }, x)
  }

  private def remap(invoke: Invoke, x: PrimitiveFunction): PrimitiveFunction = {
    pos(x match {
      case Length(a)             => Length(remap(invoke, a))
      case Lock(a)             => Lock(remap(invoke, a))
      case Holds(e, t)           => Holds(remap(invoke, e), remap(invoke, t))
      case IsLocal(e, t)         => IsLocal(remap(invoke, e), remap(invoke, t))
      case IsShared(e)           => IsShared(remap(invoke, e))
      case IsFresh(e)           => IsFresh(remap(invoke, e))
      case MoverPermission(e, v) => MoverPermission(remap(invoke, e), v.map(remap(invoke, _)))
      case GoesWrong(e) => GoesWrong(remap (invoke, e))
      case Rand()                => Rand()
      case NextSpecStep(n) => NextSpecStep(n)

    }, x)
  }

  private def remap(invoke: Invoke, x: Type): Type = {
    pos(x match {
      case ArrayType(enclosing, ident, thisVar) =>
        ArrayType(enclosing, ident, remap(invoke, thisVar))
      case _ => (new DeepCopyWithPositions)(x)
    }, x)
  }

  private def remap(invoke: Invoke, x: Spec): Spec = {
    pos(Spec(remap(invoke, x.conditionalMover), x.blocking, x.yieldsAs.map(remap(invoke, _))), x)
  }

  private def remap(invoke: Invoke, x : Const) : Const = {
    pos(x match {
      case NullConst(t)  => {
        NullConst(remap(invoke, t).asInstanceOf[RefType])
      }
      case _ => (new DeepCopyWithPositions)(x)
    }, x.pos)
  }

    private def remap(invoke: Invoke, x: Expr): Expr = {
      pos(x match {
        case ConstExpr(c)             => ConstExpr(remap(invoke, c))
        case BinaryExpr(lhs, rhs, op) => BinaryExpr(remap(invoke, lhs), remap(invoke, rhs), DeepCopyWithPositions(op))
        case UnaryExpr(expr, op)      => UnaryExpr(remap(invoke, expr), DeepCopyWithPositions(op))
        case Quantified(q, decls, pred, triggers)      => Quantified(q, decls.map(remap(invoke, _)), remap(invoke, pred), triggers.map(_.map(remap(invoke, _))))
        case Cond(p, tt, ff)          => Cond(remap(invoke, p), remap(invoke, tt), remap(invoke, ff))
        case p: Location              => remap(invoke, p)
        case x: PrimitiveFunction     => remap(invoke, x)
        case LoweredExpr(e, original) => LoweredExpr(remap(invoke, e), remap(invoke, original))
        case Old(l)                => Old(remap(invoke, l))
        case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(remap(invoke, _)), args.map(remap(invoke, _)))
      }, x)
    }
}

