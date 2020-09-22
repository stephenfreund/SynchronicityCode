

package anchor.sink
import AST._
import anchor.util.Errors
import anchor.util.Errors.fail

import scala.util.parsing.input.NoPosition

class BuildScope() {

  private def makeClassType(scope: SymTab, x: ClassDecl) = {
    val t = ClassType(x.name)
    t.decl = x
    t.scope = scope
    t
  }

  def annotate(x: Program): Unit = {
    x.scope = new SymTab(x.classes, x.globals, x.library)
    x.library.functions.foreach(annotate(x.scope, _))
    x.classes.foreach(annotate(x.scope, _))
    x.globals.foreach(g => {
      g.scope = x.scope
      annotate(x.scope, g.t)
    })

    x.axioms.foreach(g => {
      g.scope = x.scope
      annotate(x.scope, g)
    })
  }

  private def annotate(scope: SymTab, x: BuiltInFunctionDecl) : Unit = {
    x.scope = scope
    annotate(scope, x.returnType)
    x.parameters.foreach(annotate(scope, _))
  }


  private def annotate(scope: SymTab, x: ClassDecl): Unit = {
    var bodyScope = scope.pushFields(x.fields).pushMethods(x.methods).pushArrays(x.arrays)

    val thisType = makeClassType(x.scope, x)
    val thisDecl = VarDecl(thisType, "this")

    val tidType = TidType()
    val tidDecl = VarDecl(tidType, "tid")

    bodyScope = bodyScope.pushLocals(List(thisDecl, tidDecl))

    thisType.scope = bodyScope
    tidType.scope = bodyScope
    thisDecl.scope = bodyScope
    tidDecl.scope = bodyScope

    x.scope = bodyScope
    x.arrays.foreach(annotate(bodyScope, _))
    x.fields.foreach(annotate(bodyScope, _))
    x.methods.foreach(annotate(bodyScope, _))
    x.invariants.foreach(annotate(bodyScope, _))
  }

  private def annotate(scope: SymTab, x : ClassInvariant): Unit  = {
    annotate(scope, x.pred)
    x.triggers.foreach(_.foreach(annotate(scope, _)))
  }


  def annotate(scope: SymTab, x: FieldModifier) : Unit = {
    x.scope = scope
    x match {
      case VolatileModifier() =>
      case ABAFreeModifier()  =>
      case InternalModifier() =>
      case HasCASOperationModifier() =>
    }
  }

  private def annotate(scope: SymTab, x: FieldDecl): Unit = {
    x.scope = scope
    annotate(scope, x.t)
    annotateSpec(scope, x)
    for (m <- x.modifiers) annotate(scope, m)
  }

  private def annotate(scope: SymTab, x: ArrayDecl): Unit = {
    x.scope = scope
    annotate(scope, x.elemType)
    annotateSpec(scope, x)
  }

  private def annotateSpec(scope: SymTab, f: FieldDecl): Unit = {
    val x = f.spec
    annotate(scope, x, f.t)
  }

  private def annotateSpec(scope: SymTab, a : ArrayDecl): Unit = {
    val x = a.spec

    val thisDecl = scope.resolveVar("this").get

    val currentClassName = thisDecl.t.asInstanceOf[ClassType].name

    val varThis = new VarAccess(thisDecl)

    val arrayType = ArrayType(currentClassName, a.name, varThis)
    annotate(scope, arrayType)

    arrayType.decl = a
    val athisDecl = pos(VarDecl(pos(arrayType, a.pos), "athis"), a.pos)
    annotate(scope, athisDecl)

    val indexDecl = pos(VarDecl(pos(IntType(), a.pos), a.elemName), a.pos)
    annotate(scope, indexDecl)

    val bodyScope = scope.pushLocals(List(athisDecl, indexDecl))
    x.scope = bodyScope

    annotate(bodyScope, x, arrayType.elemType())

  }


  private def annotate(scope: SymTab, x: Spec, valueType : Type): Unit = {

    x.scope = scope

    val nuDecl = pos(VarDecl(valueType, "newValue"), x.pos)
    this.annotate(scope, nuDecl)

    abstract class AccessType
    case class Unk() extends AccessType
    case class Read() extends AccessType
    case class Write() extends AccessType

    def annotate(scope: SymTab, x: Expr, access: AccessType): Unit = {
      x.scope = scope
      x match {
        case ConstExpr(MoverConst(I())) if access != Write() =>
          Errors.error("Scope", "Can only refer to mover I for writes in spec", x)
        case x@ConstExpr(const)            => this.annotate(scope, x)
        case BinaryExpr(lhs, rhs, op)      => {
          annotate(scope, lhs, access)
          this.annotate(scope, op)
          annotate(scope, rhs, access)
        }
        case UnaryExpr(rhs, op)         => {
          this.annotate(scope, op)
          annotate(scope, rhs, access)
        }
        case VarAccess("newValue") if access != Write() => {
          Errors.error("Scope", "Can only refer to newValue for writes in spec", x)
        }
        case VarAccess(_) =>

        case FieldAccess(v, f) => annotate(scope, v, access)
        case ArrayAccess(l, index) =>
          annotate(scope, l, access)
          annotate(scope, index, access)
        case Old(l) => annotate(scope, l, access)
        case BuiltInFunctionCall(ident, typeArgs, args) => {
          args.foreach(annotate(scope, _, access))
          typeArgs.foreach(this.annotate(scope, _))
        }
        case Length(a)                 =>
          annotate(scope, a, access)
        case IsLocal(expr, tid)        =>
          annotate(scope, expr, access)
          annotate(scope, tid, access)
        case IsShared(expr)            =>
          annotate(scope, expr, access)
        case Holds(expr, tid)          =>
          annotate(scope, expr, access)
          annotate(scope, tid, access)
        case MoverPermission(loc, v) => {
          annotate(scope, loc, access)
          v.map(annotate(scope, _, access))
        }
        case GoesWrong(mover)       => {
          annotate(scope, mover, access)
        }

        case Rand()                    => { }

        case Quantified(_, decls, pred, triggers)         => {
          this.annotate(scope, decls)
          val newScope = scope.pushLocals(decls)
          annotate(newScope, pred, access)
          triggers.foreach(_.foreach(annotate(newScope, _, access)))
        }
        case Cond(p@VarAccess("isRead"), t, f)  => {
          annotate(scope, p, access)
          access match {
            case Write() => {
              annotate(scope, t, Write())
              annotate(scope, f, Write())
            }
            case Read()  => {
              annotate(scope, t, Read())
              annotate(scope, f, Write())
            }
            case Unk()   => {
              annotate(scope, t, Read())
              annotate(scope, f, Write())
            }
          }
        }
        case Cond(p, t, f) => {
          annotate(scope, p, access)
          annotate(scope, t, access)
          annotate(scope, f, access)
        }
      }
    }

    val isRead = VarDecl(BoolType(), "isRead")
    this.annotate(scope, isRead)

    annotate(scope.pushLocals(List(isRead, nuDecl)), x.conditionalMover, Unk())
    x.yieldsAs.foreach(this.annotate(scope.pushLocals(List(nuDecl)), _))
  }

  private def annotate(scope: SymTab, x: Location): Unit = {
    x.scope = scope
    x match {
      case VarAccess(v)      =>
      case FieldAccess(v, f) => annotate(scope, v)
      case ArrayAccess(l, index) =>
        annotate(scope, l)
        annotate(scope, index)
    }
  }


  private def annotate(scope: SymTab, x: ExplicitMethodSpec): SymTab = {
    x.requires.foreach(annotate(scope, _))
    x.vars.foreach(annotate(scope, _))
    val newScope = scope.pushLocals(x.vars)
    x.transactions.foreach(annotate(newScope, _))
    newScope
  }


  private def annotate(scope: SymTab, x: Transaction): Unit = {
    x.modifies.foreach(annotate(scope, _))
    x.ensures.foreach(annotate(scope, _))
  }

  private def annotate(scope: SymTab, x: MethodDecl): Unit = {
    x.scope = scope
    annotate(x.scope, x.returnType)
    x.params.foreach(annotate(x.scope, _))
    var methodScope = x.scope.pushLocals(x.params)
//    if (x.returnType != VoidType()) {
//      val returnVar = VarDecl(x.returnType, "$result")
//      this.annotate(scope, returnVar)
//      methodScope = methodScope.pushLocals(List(returnVar))
//    }
    val methodPlusSpecScope = annotate(methodScope, x.spec)
    annotate(methodPlusSpecScope, x.stmt)
  }



  private def annotate(scope: SymTab, x: VarDecl): SymTab = {
    x.scope = scope
    annotate(scope, x.t)
    scope.pushLocals(List(x))
  }

  private def annotate(scope: SymTab, xs: List[VarDecl]): SymTab = {
    for (x <- xs) {
      annotate(scope, x.t)
      x.scope = scope
    }
    scope.pushLocals(xs)
  }

  private def annotate(scope: SymTab, x: Stmt): SymTab = {
    x.scope = scope
    x match {
      case VarDeclStmt(v)             => {
        annotate(scope, v)
      }
      case Assign(lhs, rhs)           => {
        lhs.map(annotate(scope, _))
        rhs.map(annotate(scope, _))
        scope
      }
      case Block(name, body) => {
        body.foldLeft(scope)(annotate)
        scope
      }
      case Write(lhs, field, rhs, _)     => {
        annotate(scope, lhs)
        annotate(scope, rhs)
        scope
      }
      case LocalWrites(writes)        => {
        writes.foreach(annotate(scope, _))
        scope
      }
      case Read(lhs, rhs, field, _)  => {
        annotate(scope, lhs)
        annotate(scope, rhs)
        scope
      }
      case Invoke(ref, method, args, Some(v), invariants) => {
        annotate(scope, ref)
        args.foreach(annotate(scope, _))
        annotate(scope, v)
        invariants.foreach(annotate(scope, _))
        scope
      }
      case Invoke(ref, method, args, None, invariants) => {
        annotate(scope, ref)
        args.foreach(annotate(scope, _))
        invariants.foreach(annotate(scope, _))
        scope
      }
      case InlineInvoke(invoke) =>
        annotate(scope, invoke)
        scope
      case InlineReturn() =>
        scope
      case Return(None, _) => { scope }
      case Return(Some(e), _) => {
        annotate(scope, e)
        scope
      }
      case Sync(lock, stmt, _) => {
        annotate(scope, lock)
        annotate(scope, stmt)
        scope
      }
      case If(cond, t, f) => {
        annotate(scope, cond)
        annotate(scope, t)
        annotate(scope, f)
        scope
      }
      case While(cond, stmt, invs, decreases) => {
        invs.foreach(annotate(scope, _))
        annotate(scope, cond)
        annotate(scope, stmt)
        decreases.foreach(annotate(scope, _))
        scope
      }
      case Break(label) => { scope }

      case CAS(result, lhs, field, expected, rhs) => {
        annotate(scope, lhs)
        annotate(scope, expected)
        annotate(scope, rhs)
        annotate(scope, result)
        scope
      }
      case Alloc(lhs, name)                       => {
        annotate(scope, lhs)
        annotate(scope, name)
        scope
      }
      case AWrite(lhs, index, rhs)                => {
        annotate(scope, lhs)
        annotate(scope, rhs)
        annotate(scope, index)
        scope
      }
      case ARead(lhs, rhs, index)                 => {
        annotate(scope, lhs)
        annotate(scope, rhs)
        annotate(scope, index)
        scope
      }
      case AAlloc(lhs, t, size) => {
        annotate(scope, lhs)
        annotate(scope, t)
        annotate(scope, size)
        scope
      }
      case Yield(ensures)       =>
        ensures.foreach(annotate(scope, _))
        scope
      case Commit() =>
        scope
      case Assume(expr)         =>
        annotate(scope, expr)
        scope
      case Assert(expr)         =>
        annotate(scope, expr)
        scope
      case Invariant(expr)      =>
        annotate(scope, expr)
        scope
      case BoogieCode(_)        =>
        scope

      case NoReductionCheck(s) => {
        annotate(scope, s)
        scope
      }
      case Acquire(expr)       => {
        annotate(scope, expr)
        scope
      }
      case Release(expr)       => {
        annotate(scope, expr)
        scope
      }
    }
  }

  private def annotate(scope: SymTab, x: Const): Unit = {
    x.scope = scope
    x match {
      case NullConst(t) => annotate(scope, t)
      case EmptyCollectionConst(t) => annotate(scope, t)
      case _            =>
    }
  }

  private def annotate(scope: SymTab, x: VarAccess): Unit = {
    x.scope = scope
  }

  private def annotate(scope: SymTab, x: Expr): Unit = {
    x.scope = scope
    x match {
      case ConstExpr(c) => {
        annotate(scope, c)
      }
      case BinaryExpr(lhs, rhs, op) => {
        annotate(scope, lhs)
        annotate(scope, op)
        annotate(scope, rhs)
      }
      case UnaryExpr(expr, op) => {
        annotate(scope, expr)
        annotate(scope, op)
      }
      case Quantified(_, decls, pred, triggers) => {
        annotate(scope, decls)
        val newScope = scope.pushLocals(decls)
        annotate(newScope, pred)
        triggers.foreach(_.foreach(annotate(newScope, _)))

      }
      case Cond(p, tt, ff) => {
        annotate(scope, p)
        annotate(scope, tt)
        annotate(scope, ff)
      }
      case x : Location => {
        annotate(scope, x)
      }
      case x : PrimitiveFunction => {
        annotate(scope, x)
      }
      case LoweredExpr(e, orig) => {
        annotate(scope, e)
        annotate(scope, orig)
      }
      case Old(l) => {
        annotate(scope, l)
      }
      case BuiltInFunctionCall(ident, typeArgs, args) => {
        args.foreach(annotate(scope, _))
        typeArgs.foreach(annotate(scope, _))
      }

    }
  }

  private def annotate(scope: SymTab, x: BinaryOp): Unit = {
    x.scope = scope
  }

  private def annotate(scope: SymTab, x: UnaryOp): Unit = {
    x.scope = scope
  }

  private def annotate(scope: SymTab, x: Type): Unit = {
    x.scope = scope
    x match {
      case IntType()                                 =>
      case TidType()                                 =>
      case VoidType()                                =>
      case BoolType()                                =>
      case MoverType()                               =>
      case BoogieType(name)                          =>
      case ClassType(ident)                          =>
      case x@ArrayType(enclosing, ident, thisAccess) =>
        annotate(scope, thisAccess)
      case x@CollectionType(_, _)                      =>
        x.typeArgs.foreach(annotate(scope, _))
      case TypeVar(_)                                =>

    }
  }

  private def annotate(scope: SymTab, x: PrimitiveFunction): Unit = {
    x.scope = scope
    x match {
      case Length(a)                 =>
        annotate(scope, a)
      case Lock(a)                 =>
        annotate(scope, a)
      case IsLocal(expr, tid)        =>
        annotate(scope, expr)
        annotate(scope, tid)
      case IsShared(expr)            =>
        annotate(scope, expr)
      case IsFresh(expr)            =>
        annotate(scope, expr)
      case Holds(expr, tid)          =>
        annotate(scope, expr)
        annotate(scope, tid)
      case MoverPermission(loc, v) => {
        annotate(scope, loc)
        v.map(annotate(scope, _))
      }
      case GoesWrong(mover)       => {
        annotate(scope, mover)
      }

      case Rand()                    => { }
      case NextSpecStep(_) => {}
    }
  }
}

object BuildScope {
  def annotate(x: Program): Unit = {
    new BuildScope().annotate(x)
  }
  def annotate(scope: SymTab, x : Expr): Unit = {
    new BuildScope().annotate(scope, x)
  }
  def annotate(scope: SymTab, x : Type): Unit = {
    new BuildScope().annotate(scope, x)
  }
  def annotate(scope: SymTab, x : Spec, valueType: Type): Unit = {
    new BuildScope().annotate(scope, x, valueType)
  }
}
