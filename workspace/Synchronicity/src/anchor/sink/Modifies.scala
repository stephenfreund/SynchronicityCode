package anchor.sink

sealed abstract class ModifiedElem
case class FieldMod(fd : FieldDecl) extends ModifiedElem {
  override def toString: String = s"Field(${fd.parent.name}.${fd.name})"
}
case class ArrayMod(a : ArrayType) extends ModifiedElem {
  override def toString: String = s"Array(${PrettyPrint.pp(a)})"
}
case class LocalMod(v : VarDecl) extends ModifiedElem {
  override def toString: String = s"Local(${v.name})"
}
case class LockMod(r : RefType) extends ModifiedElem {
  override def toString: String = s"Lock(${PrettyPrint.pp(r)})"
}
case class CASMod(r : RefType) extends ModifiedElem {
  override def toString: String = s"CAS(${PrettyPrint.pp(r)})"
}
case class SharedMod(r : RefType) extends ModifiedElem {
  override def toString: String = s"Shared(${PrettyPrint.pp(r)})"
}
case class AllMod() extends ModifiedElem {
  override def toString: String = s"ALL()"
}
//case class PrimitiveMod(p : PrimitiveFunction) extends ModifiedElem

object Modifies {

  private def shareMod(v: VarOrConst) : Option[SharedMod] = {
    v match {
      case v@VarAccess(name) => {
        v.t match {
          case refType: RefType => Some(SharedMod(refType))
          case _                => None
        }
      }
      case _                 => None
    }
  }

  private def modifies(s: Stmt): Set[ModifiedElem] = {
    s match {
      case x@VarDeclStmt(decl)                                 => Set()
      case x@Assign(lhs, rhs)                                  => lhs.map(x => LocalMod(x.decl)).toSet
      case x@Block(label, body)                       => body.flatMap(modifies(_)).toSet
      case x@Write(ref, field, rhs, _)                            =>
        Set(FieldMod(x.decl)) ++ shareMod(rhs)
      case x@Read(lhs, ref, field, _)                             => Set(LocalMod(lhs.decl))
      case x@AWrite(ref, index, rhs)                           =>
        Set(ArrayMod(ref.t.asInstanceOf[ArrayType])) ++ shareMod(rhs)
      case x@LocalWrites(writes)                               => writes.map(modifies(_)).reduce(_ ++ _)
      case x@ARead(lhs, ref, index)                            => Set(LocalMod(lhs.decl))
      case x@CAS(result, ref, field, expected, rhs: VarAccess) =>
        Set(LocalMod(rhs.decl),
          FieldMod(x.decl)) ++ shareMod(rhs) ++ Set(CASMod(ClassType(x.decl.parent)))
      case x@CAS(result, ref, field, expected, rhs: ConstExpr) =>
        Set(FieldMod(x.decl))
      case x@Invoke(ref, method, args, result, invariants)     => Set(AllMod())
      case InlineInvoke(invoke) => Set()
      case InlineReturn()                                      => Set()

      case x@Return(result, _)                        => Set()
      case x@If(cond, trueBranch, falseBranch)        => modifies(trueBranch) ++ modifies(falseBranch)
      case x@While(cond, stmt, invariants, decreases) => modifies(stmt)
      case x@Break(label)                             => Set()
      case x@Alloc(lhs, name)                         => Set(LocalMod(lhs.decl)) ++ shareMod(lhs)
      case x@AAlloc(lhs, t, size)                     => Set(LocalMod(lhs.decl)) ++ shareMod(lhs)
      case x@Assume(expr)                             => Set()
      case x@Assert(expr)                             => Set()
      case x@Invariant(expr)                          => Set()
      case x@Yield(ensures)                           => Set(AllMod())
      case x@Commit()                                 => Set()
      case x@BoogieCode(code)                         => Set(AllMod())
      case x@NoReductionCheck(stmt)                   => modifies(stmt)
      case x@Sync(lock, stmt, releasePos)             => Set(LockMod(lock.t.asInstanceOf[RefType]))
      case x@Acquire(lock)                            => Set(LockMod(lock.t.asInstanceOf[RefType]))
      case x@Release(lock)                            => Set(LockMod(lock.t.asInstanceOf[RefType]))
    }
  }

  private def getTidDecl(x: ASTNode) = {
    new VarAccess(x.scope.resolveVar("tid").get)
  }

  private def kills(mods: Set[ModifiedElem], location: Location): Boolean = mods.contains(AllMod()) || (location match {
    case x@VarAccess(name) => {
      mods.exists(_ match {
        case LocalMod(v) => v eq x.decl
        case _           => false
      })
    }
    case x@FieldAccess(l, name)  => kills(mods, l) || mods.contains(FieldMod(x.decl))
    case x@ArrayAccess(l, index) => kills(mods, l) || kills(mods, index) || mods.contains(ArrayMod(x.l.t.asInstanceOf[ArrayType]))

  })

  private def kills(mods: Set[ModifiedElem], f: PrimitiveFunction): Boolean = {
    mods.contains(AllMod()) || (
      f match {
        case Length(a)                 => kills(mods, a)
        case Lock(a)                 => kills(mods, a)
        case IsLocal(loc, tid)         => kills(mods, loc) || kills(mods, tid) || mods.contains(SharedMod(loc.t.asInstanceOf[RefType]))
        case IsShared(loc)             => kills(mods, loc) || mods.contains(SharedMod(loc.t.asInstanceOf[RefType]))
        case IsFresh(loc)             => kills(mods, loc) || mods.contains(SharedMod(loc.t.asInstanceOf[RefType]))
        case Holds(loc, tid)           => kills(mods, loc) || kills(mods, tid) || mods.contains(LockMod(loc.t.asInstanceOf[RefType]))
        case MoverPermission(loc, v) => kills(mods, loc) || mods.contains(CASMod(loc.t.asInstanceOf[RefType])) ||
          v.exists(kills(mods, _))
        case GoesWrong(e) => kills(mods, e)

        case Rand()                    => false
        case NextSpecStep(n) => false

      })
  }

  def kills(mods: Set[ModifiedElem], e: Expr): Boolean = mods.contains(AllMod()) || (e match {
    case ConstExpr(const)            => false
    case BinaryExpr(lhs, rhs, op)    => kills(mods, lhs) || kills(mods, rhs)
    case UnaryExpr(expr, op)         => kills(mods, expr)
    case Quantified(_, decls, pred, triggers)  => kills(mods, pred) || triggers.exists(_.exists(kills(mods, _)))
    case Cond(p, tt, ff)             => kills(mods, p) || kills(mods, tt) || kills(mods, ff)
    case location: Location          => kills(mods, location)
    case function: PrimitiveFunction => kills(mods, function)
    case LoweredExpr(e, orig)        => kills(mods, e)
    case x@Old(l) => kills(mods, l)
    case BuiltInFunctionCall(name, types, args) => args.exists(kills(mods, _))

  })

  def killLocalInvariants(s : Stmt, es : List[Expr]): List[Expr] = {
    val killSet = modifies(s)
    es.filter(e => kills(killSet, e))
  }
//
//  def makeObjectInvariantFilter(s: Stmt) = {
//    val killSet = modifies(s)
//    (e : Expr, owner: ClassDecl) =>
//      kills(killSet, e) || killSet.contains(SharedMod(ClassType(owner)))
//  }

  class InvariantFilter(s : Option[Stmt]) {
    val killSet : Set[ModifiedElem] = s match {
      case Some(value) => {
        Modifies.modifies(value)
      }
      case None        => {
        Set[ModifiedElem](AllMod())
      }
    }

  def killsLocalInvariant(e: Expr) = {
      kills(killSet, e)
    }
    def killsObjectInvariant(e: Expr, owner: ClassDecl) = {
      val b = kills(killSet, e) || killSet.contains(SharedMod(ClassType(owner)))
//      if (! killSet.contains(AllMod())) println(s"${killSet} ${PrettyPrint.pp(e)} => ${b}")
      b
    }
  }
}

