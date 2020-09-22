//
// Lowering does the following:
//   - replaces sync(x) { ... } with acq(x); ... rel(x); and also puts releases at all exits from ...
//   - changes breaks into jumps to labels
//

package anchor.sink

import AST.pos
import acme.scala.Util
import anchor.transforms.DeepCopyWithPositions
import anchor.util.Errors
import anchor.util.Errors._

private sealed abstract class ContextItem
private case class SynchItem(val release : Release) extends ContextItem
private case class LabelItem(val label : String) extends ContextItem
private case class LoopItem() extends ContextItem

class Context private(val method: Option[MethodDecl], private val history : List[ContextItem]) {

  def this(method : Option[MethodDecl]) = {
    this(method, List(LabelItem(acme.scala.Util.fresh("block"))))
  }

  def this(method: MethodDecl) = {
    this(Some(method))
  }

  def this() = {
    this(None)
  }

  def synch(release : Release) = new Context(method, SynchItem(release) :: this.history)

  def block(label : String) = {
    new Context(method, LabelItem(label) :: this.history)
  }

  def loop() = new Context(method, LoopItem() :: this.history)

  def releasesInBlock(label: String) = {
    assert(history.contains(LabelItem(label)))
    history.takeWhile(_ != LabelItem(label)).filter(_.isInstanceOf[SynchItem]).map(_.asInstanceOf[SynchItem].release).map(r => pos(Release(pos(VarAccess(r.lock.name), r.lock.pos)), r.pos))
  }

  def releasesInTopLoop() = {
    history.takeWhile(!_.isInstanceOf[LoopItem]).filter(_.isInstanceOf[SynchItem]).map(_.asInstanceOf[SynchItem].release).map(r => pos(Release(pos(VarAccess(r.lock.name), r.lock.pos)), r.pos))
  }

  def releases() = {
    history.filter(_.isInstanceOf[SynchItem]).map(_.asInstanceOf[SynchItem].release).map(r => pos(Release(pos(VarAccess(r.lock.name), r.lock.pos)), r.pos))
  }

}


class LowerAST() {


  def apply(x: Program): Program = {
    pos(Program(x.classes.map(c => this (c)), x.globals.map(c => this (c)),  x.axioms.map(this(_)), new Library(x.library.collections.mapValues(this(_)))), x.pos)
  }

  def apply(x: Collection) : Collection = {
    new Collection(x.name, x.numParams, x.functions.map(this (_)), x.boogieText)
  }

  def apply(x: BuiltInFunctionDecl) : BuiltInFunctionDecl = {
    pos(BuiltInFunctionDecl(x.name, x.typeVars, x.parameters.map(this(_)), this(x.returnType)), x.pos)
  }


  def apply(x: ClassDecl): ClassDecl = {
    pos(ClassDecl(x.name, x.arrays.map(this (_)), x.fields.map(this (_)), x.methods.map(this (_)), x.invariants.map(this (_))), x.pos)
  }

  def apply(x : ClassInvariant) : ClassInvariant = {
    pos(ClassInvariant(this(x.pred), x.triggers.map(_.map(this(_)))), x.pos)
  }


  def apply(x: ArrayDecl): ArrayDecl = {
    pos(ArrayDecl(x.name, this (x.elemType), x.elemName, this (x.spec)), x.pos)
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
    pos(FieldDecl(this (x.t), x.name, this (x.spec), x.modifiers.map(this(_))), x.pos)
  }


  private def apply(x: Spec): Spec = {

    abstract class AccessType
    case class Unk() extends AccessType
    case class Read() extends AccessType
    case class Write() extends AccessType

    def check(x: Expr, access: AccessType): Expr = {
      pos(x match {
        case VarAccess(name)       => VarAccess(name)
        case FieldAccess(l, name)  => FieldAccess(check(l, access), name)
        case ArrayAccess(l, index) => ArrayAccess(check(l, access), check(index, access))
        case x@ConstExpr(const)                => ConstExpr(const)
        case BinaryExpr(lhs, rhs, op)          => {
          BinaryExpr(check(lhs, access), check(rhs, access), DeepCopyWithPositions(op))
        }
        case UnaryExpr(rhs, op)                => {
          UnaryExpr(check(rhs, access), DeepCopyWithPositions(op))
        }
        case l : Location                      => check(l, access)
        case Length(a)                         => Length(check(a, access))
        case Lock(a)                         => Lock(check(a, access))
        case IsLocal(expr, tid)                => IsLocal(check(expr, access), check(tid, access))
        case IsShared(expr)                    => IsShared(check(expr, access))
        case IsFresh(expr)                    => IsFresh(check(expr, access))
        case Holds(expr, tid)                  => Holds(check(expr, access), check(tid, access))
        case MoverPermission(loc, v)         => MoverPermission(check(loc, access), v.map(check(_, access)))
        case GoesWrong(e) => GoesWrong(check(e, access))
        case Rand()                            => Rand()
        case NextSpecStep(n) => NextSpecStep(n)
        case Quantified(q, decls, pred, triggers)     => Quantified(q, decls.map(DeepCopyWithPositions(_)), check(pred, access), triggers.map(_.map(check(_, access))))
        case Cond(p@VarAccess("isRead"), t, f) => {
          val cond = check(p, access)
          access match {
            case Write() => {
              Errors.warn("Cond Mover", "Unreachable case in Spec", t)
              //check(t, Write()) // to ensure scope doesn't get newValueDecl twice...
              check(f, Write())
            }
            case Read()  => {
              Errors.warn("Cond Mover", "Unreachable case in Spec", f)
              check(t, Read())
              //check(f, Write())
            }
            case Unk()   => {
              Cond(p, check(t, Read()), check(f, Write()))
            }
          }
        }
        case Cond(p, t, f)                     => Cond(check(p, access), check(t, access), check(f, access))
        case LoweredExpr(e, original) => LoweredExpr(check(e, access), DeepCopyWithPositions(original))
        case Old(l)                => Old(check(l, access))
        case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(DeepCopyWithPositions(_)), args.map(check(_, access)))
      }, x.pos)
    }

    pos(Spec(check(x.conditionalMover, Unk()), x.blocking, x.yieldsAs.map(this (_))), x.pos)
  }

  def apply(x: VarAccess): VarAccess = {
    pos(VarAccess(x.name), x.pos)
  }

  def apply(x: Location): Location = {
    pos(x match {
      case VarAccess(l)          => VarAccess(l)
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
    pos(MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), this(x.spec), this (new Context(x), x.stmt)),
      x.pos)
  }

  def apply(x: VarDecl): VarDecl = {
    pos(VarDecl(this (x.t), x.name), x.pos)
  }

  def apply(context: Context, x: Stmt): Stmt = {
    pos(x match {
      case Block(name, body)         => {
        var newContext = name match {
          case None       => context
          case Some(name) => context.block(name)
        }
        Block(name, body.map(this (newContext, _)))
      }

      case InlineInvoke(invoke) =>
        InlineInvoke(this(context, invoke).asInstanceOf[Invoke])
      case InlineReturn() =>
        InlineReturn()

      case Sync(lock, stmt, relPos)           => {
        Block(None, List(pos(Acquire(this(lock)), lock.pos),
          this (context.synch(pos(Release(this(lock)), relPos)), stmt),
          pos(Release(this(lock)), relPos)))
      }
      case If(cond, t, f)                     => {
        If(this (cond), this (context, t), this (context, f))
      }
      case While(cond, stmt, invs, decreases) => {
        While(this (cond),
          this (context.loop(), stmt),
          (invs ++ context.releases().map(r => pos(Holds(r.lock, pos(VarAccess("tid"), x.pos)), x.pos))).map(this (_)),
          decreases.map(this (_))
        )
      }

      case Break(label) => {
        val releases = label match {
          case None        => context.releasesInTopLoop()
          case Some(label) => context.releasesInBlock(label)
        }
        Block(None, releases ++ List(x))
      }

      case Return(e, isSynthetic) => {
        val releases = context.releases()
        Block(None, releases ++ List(pos(Return(e.map(this (_)), isSynthetic), x.pos)))
      }

      case VarDeclStmt(v)                             => VarDeclStmt(this (v))
      case Assign(lhs, rhs)                           => Assign(lhs.map(this (_)), rhs.map(this (_)))
      case Write(lhs, field, rhs, movesAs)            => Write(this (lhs), field, this (rhs), movesAs)
      case LocalWrites(writes)                        => LocalWrites(writes.map(this (context, _).asInstanceOf[Write]))
      case Read(lhs, rhs, field, movesAs)                      => Read(this (lhs), this (rhs), field, movesAs)
      case CAS(result, lhs, field, expected, rhs)     => CAS(this (result), this (lhs), field, this (expected), this (rhs))
      case Invoke(ref, method, args, res, invariants) => Invoke(this (ref), method, args.map(this (_)), res.map(this (_)), invariants.map(this (_)))
      case Alloc(lhs, name)                           => Alloc(this (lhs), name)
      case Yield(ensures)                             => Yield(ensures.map(this (_)))
      case Commit()                                   => Commit()
      case Assume(x)                                  => Assume(this (x))
      case Assert(x)                                  => Assert(this (x))
      case Invariant(x)                               => Invariant(this (x))
      case AWrite(lhs, i, rhs)                        => AWrite(this (lhs), this (i), this (rhs))
      case ARead(lhs, rhs, i)                         => ARead(this (lhs), this (rhs), this (i))
      case AAlloc(lhs, a, size)                       => AAlloc(this (lhs), this (a).asInstanceOf[ArrayType], this (size))
      case BoogieCode(s)                              => BoogieCode(s)
      case NoReductionCheck(s)                        => NoReductionCheck(this (context, s))
      case Acquire(x)                                 => Acquire(this (x))
      case Release(x)                                 => Release(this (x))
    }, x.pos)
  }

  def apply(x: VarOrConst) : VarOrConst = {
    pos(x match {
      case y@ConstExpr(const) => this(y)
      case y@VarAccess(name) => this(y)
    }, x.pos)
  }

  def apply(x : ConstExpr) : ConstExpr = {
    pos(ConstExpr(this (x.const)), x.pos)
  }

  def apply(x: Const): Const = {
    pos(x match {
      case IntConst(v)   => IntConst(v)
      case BoolConst(v)  => BoolConst(v)
      case NullConst(t)  => NullConst(this(t).asInstanceOf[RefType])
      case NullTid()     => NullTid()
      case MoverConst(m) => MoverConst(m)
      case EmptyCollectionConst(t) => EmptyCollectionConst(this(t).asInstanceOf[CollectionType])
    }, x.pos)
  }

  def apply(x: Expr): Expr = {
    pos(x match {
      case ConstExpr(c)             => ConstExpr(this (c))
      case BinaryExpr(lhs, rhs, op) => BinaryExpr(this (lhs), this (rhs), op)
      case UnaryExpr(expr, op)      => UnaryExpr(this (expr), op)
      case Cond(p, tt, ff)          => Cond(this (p), this (tt), this (ff))
      case Quantified(q, decls, pred, triggers)      => Quantified(q, decls.map(this (_)), this (pred), triggers.map(_.map(this(_))))
      case p: Location              => this (p)
      case x: PrimitiveFunction     => this (x)
      case LoweredExpr(e, orig)     => LoweredExpr(this (e), this (orig))
      case Old(l)                => Old(this (l))
      case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(this(_)), args.map(this(_)))
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
      case MoverPermission(l, v) => MoverPermission(this (l), v.map(apply))
      case GoesWrong(e) => GoesWrong(this (e))
      case Rand()                => Rand()
      case NextSpecStep(n) => NextSpecStep(n)
    }, x.pos)
  }


  def apply(x: Type): Type = {
    pos(x match {
      case IntType()                            => IntType()
      case TidType()                            => TidType()
      case VoidType()                           => VoidType()
      case BoolType()                           => BoolType()
      case BoogieType(name)                     => BoogieType(name)
      case MoverType()                          => MoverType()
      case ClassType(ident)                     => ClassType(ident)
      case ArrayType(enclosing, ident, thisVar) => ArrayType(enclosing, ident, this (thisVar))
      case TypeVar(x) => TypeVar(x)
      case CollectionType(name, args) => CollectionType(name, args.map(this(_)))
    }, x.pos)
  }


  //  def lower(x: Expr): (Expr, List[VarDeclStmt], List[Stmt]) = {
//    val r = x match {
//      case ConstExpr(c)             => (ConstExpr(this(c)), Nil, Nil)
//      case BinaryExpr(lhs, rhs, op) => {
//        val (lhse, ldecls, lhss) = lower (lhs)
//        val (rhse, rdecls, rhss) = lower (rhs)
//        (BinaryExpr(lhse, rhse, op), ldecls ++ rdecls, lhss ++ rhss)
//      }
//      case UnaryExpr(expr, op)      => {
//        val (lhse, ldecls, lhss) = lower (expr)
//        (UnaryExpr(lhse, op), ldecls, lhss)
//      }
//      case Cond(p, tt, ff)          => fail("Lower", "Not implemented")
//      case ForAll(decls, pred)      => fail("Lower", "Bad forall for lowering")
//      case p: Location              => lower (p)
//      case x: PrimitiveFunction     => lower (x)
//    }
//    (pos(r._1, x.pos), r._2, r._3)
//  }
//
//  private def lower(x: PrimitiveFunction): (PrimitiveFunction, List[VarDeclStmt], List[Stmt]) = {
//    val r = x match {
//      case Length(a) => {
//        val (ae, decls, as) = lower(a)
//        (Length(ae), decls, as)
//      }
//      case Rand()    => (Rand(), Nil, List[Stmt]())
//      case _         => fail("Lower", "Bad primitive for lowering")
//    }
//    (pos(r._1, x.pos), r._2, r._3)
//
//  }
//
//  def lower(x: Location): (VarAccess, List[VarDeclStmt], List[Stmt]) = {
//    var r = x match {
//      case VarAccess(l)          => (VarAccess(l), Nil, Nil)
//      case x@FieldAccess(v, field) => {
//        val (de, decls, ds) = lower(v)
//        val result = pos(VarDeclStmt(pos(VarDecl(x.decl.t, Util.fresh("tmp")), x)), x)
//        val access = pos(Read(pos(new VarAccess(result.decl), x), pos(de, x), field), x)
//        (pos(new VarAccess(result.decl), x), List(result), ds :+ access)
//      }
//
//      case x@ArrayAccess(l, i@VarAccess(index)) => {
//        val (ae, decls, as) = lower(l)
//        val rd = pos(VarDeclStmt(pos(VarDecl(x.t, Util.fresh("tmp")),x)), x)
//        val ra = pos(new VarAccess(rd.decl), x)
//        val access = pos(ARead(ra, ae, i), x)
//        val stmts = as :+ access
//        (ra, List(rd), stmts)
//      }
//
//      case x@ArrayAccess(l, index) => {
//        val (ae, adecls, as) = lower(l)
//        val (ie, idecls, is) = lower(index)
//        val id = pos(VarDeclStmt(pos(VarDecl(IntType(), Util.fresh("tmp")), index)), index)
//        val ia = pos(new VarAccess(id.decl), index)
//        val rd = pos(VarDeclStmt(pos(VarDecl(x.t, Util.fresh("tmp")),x)), x)
//        val ra = pos(new VarAccess(rd.decl), x)
//        val access = pos(ARead(ra, ae, ia), x)
//        val stmts = (as ++ is) :+ pos(Assign(ia, ie), index) :+ access
//        (ra, (adecls ++ idecls) :+ id :+ rd, stmts)
//      }
//      case Old(l)                => fail("Lower", "Bad Old for lowering")
//    }
//    (pos(r._1, x.pos), r._2, r._3)
//  }



}

object LowerAST {
  def apply(x: Program) = {
    (new LowerAST()) (x)
  }
}
