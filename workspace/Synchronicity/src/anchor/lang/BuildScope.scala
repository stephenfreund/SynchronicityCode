package anchor.lang

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
    x.scope = new SymTab(x.classes, x.globals)
    x.classes.foreach(annotate(x.scope, _))
    x.globals.foreach(g => {
      g.scope = x.scope
      annotate(x.scope, g.t)
    })
    x.axioms.foreach(g => {
      g.scope = x.scope
      annotate(x.scope, g)
    })
    for (f <- Library.functions) {
      annotate(x.scope, f)
    }
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
    annotate(bodyScope, x.constructor)
    x.invariants.foreach(annotate(bodyScope, _))
  }

  private def annotate(scope: SymTab, x : ClassInvariant): Unit  = {
    annotate(scope, x.pred)
    x.triggers.foreach(_.foreach(annotate(scope, _)))
  }



  private def annotate(scope: SymTab, x: FieldDecl): Unit = {
    x.scope = scope
    annotate(scope, x.t)
    annotateSpec(scope, x)
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

    val currentClassName = scope.resolveVar("this").get.t.asInstanceOf[ClassType].name

    val arrayType = ArrayType(currentClassName, a.name, VarAccess("this"))
    arrayType.decl = a
    annotate(scope, arrayType)

    val athisDecl = pos(VarDecl(pos(arrayType, a.pos), "athis"), a.pos)
    annotate(scope, athisDecl)

    val indexDecl = pos(VarDecl(pos(IntType(), a.pos), a.elemName), a.pos)
    annotate(scope, indexDecl)

    val bodyScope = scope.pushLocals(List(athisDecl, indexDecl))
    x.scope = bodyScope

    annotate(bodyScope, x, arrayType.elemType())

  }


  private def annotate(scope: SymTab, x: MoverSpec, valueType : Type): Unit = {

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

        case FieldAccess(v, f, _) => annotate(scope, v, access)
        case ArrayAccess(l, index) =>
          annotate(scope, l, access)
          annotate(scope, index, access)
        case Old(l) => annotate(scope, l, access)
        case BuiltInFunctionCall(ident, typeArgs, args) => {
          args.foreach(annotate(scope, _, access))
          typeArgs.foreach(_.foreach(this.annotate(scope, _)))
        }

        case Length(a)                 =>
          annotate(scope, a, access)
        case IsLocal(expr, tid)        =>
          annotate(scope, expr, access)
          annotate(scope, tid, access)
        case IsShared(expr)            =>
          annotate(scope, expr, access)
        case IsFresh(expr)            =>
          annotate(scope, expr, access)
        case Holds(expr, tid)          =>
          annotate(scope, expr, access)
          annotate(scope, tid, access)
        case NextCASSucceeds(loc, tid) => {
          annotate(scope, loc, access)
          annotate(scope, tid, access)
        }
        case Rand()                    => { }

        case Quantified(_, decls, pred, triggers) => {
          this.annotate(scope, decls)
          val newScope = scope.pushLocals(decls)
          annotate(newScope, pred, access)
          triggers.foreach(_.foreach(annotate(newScope, _, access)))

        }
        case Cond(p@VarAccess("isRead"), t, f) => {
          annotate(scope, p, access)
          access match {
            case Write() => {
              //Errors.warn("Scope", "Unreachable case in Spec", t)
              annotate(scope, t, Write()) // to ensure scope doesn't get newValueDecl twice...
              annotate(scope, f, Write())
            }
            case Read()  => {
              //Errors.warn("Scope", "Unreachable case in Spec", f)
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
      case FieldAccess(v, f, _) => annotate(scope, v)
      case ArrayAccess(l, index) =>
        annotate(scope, l)
        annotate(scope, index)
    }
  }

  private def annotate(scope: SymTab, x: MethodDecl): Unit = {
    x.scope = scope
    annotate(x.scope, x.returnType)
    x.params.foreach(annotate(x.scope, _))
    var methodScope = x.scope.pushLocals(x.params)
    val methodPlusSpecScope = annotate(methodScope, x.spec)
    annotate(methodPlusSpecScope, x.stmt)
  }

  private def annotate(scope: SymTab, x: ConstructorDecl): Unit = {
    x.scope = scope
    x.params.foreach(annotate(x.scope, _))
    var methodScope = x.scope.pushLocals(x.params)
    val methodPlusSpecScope = annotate(methodScope, x.spec)
    annotate(methodPlusSpecScope, x.stmt)
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
      case VarDeclStmt(v, e)       => {
        annotate(scope, v)
        e.foreach(annotate(scope, _))
        scope.pushLocals(List(v))
      }
      case Assign(lhs, rhs)              => {
        annotate(scope, lhs)
        annotate(scope, rhs)
        scope
      }
      case LocalAssign(assigns)          => {
        assigns.foreach(annotate(scope, _))
        scope
      }
      case Block(name, body)             => {
        body.foldLeft(scope)(annotate)
        scope
      }
      case ExprStmt(i)                   => {
        annotate(scope, i)
        scope
      }
      case Return(e, _)                  => {
        e.foreach(annotate(scope, _))
        scope
      }
      case SyncBlock(lock, stmt, _)      => {
        annotate(scope, lock)
        annotate(scope, stmt)
        scope
      }
      case If(cond, t, f)                => {
        annotate(scope, cond)
        annotate(scope, t)
        annotate(scope, f)
        scope
      }
      case While(cond, stmt, invs, decs) => {
        invs.foreach(annotate(scope, _))
        decs.foreach(annotate(scope, _))
        annotate(scope, cond)
        annotate(scope, stmt)
        scope
      }
      case Break(label)                  => { scope }

      case Yield(ensures)         =>
        ensures.foreach(annotate(scope, _))
        scope
      case Commit()          =>
        scope
      case Assume(expr)           =>
        annotate(scope, expr)
        scope
      case Assert(expr)           =>
        annotate(scope, expr)
        scope
      case Invariant(expr)        =>
        annotate(scope, expr)
        scope
      case BoogieCode(_)          =>
        scope
      case NoReductionCheck(stmt) =>
        annotate(scope, stmt)
        scope
      case SyncStmt(op, expr)          => {
        annotate(scope, op)
        annotate(scope, expr)
        scope
      }
    }
  }

  private def annotate(scope: SymTab, x: Const): Unit = {
    x.scope = scope
    x match {
      case NullConst() =>
      case _            =>
    }
  }

  private def annotate(scope: SymTab, x: VarAccess): Unit = {
    x.scope = scope
  }

  private def annotate(scope: SymTab, x: Expr): Unit = {
    x.scope = scope
    x match {
      case CAS(lhs, field, expected, rhs)    => {
        annotate(scope, lhs)
        annotate(scope, expected)
        annotate(scope, rhs)
      }
      case ConstExpr(c)                      => {
        annotate(scope, c)
      }
      case BinaryExpr(lhs, rhs, op)          => {
        annotate(scope, lhs)
        annotate(scope, op)
        annotate(scope, rhs)
      }
      case UnaryExpr(expr, op)               => {
        annotate(scope, expr)
        annotate(scope, op)
      }
      case Quantified(_, decls, pred, triggers) => {
        annotate(scope, decls)
        val newScope = scope.pushLocals(decls)
        annotate(newScope, pred)
        triggers.foreach(_.foreach(annotate(newScope, _)))
      }
      case Cond(p, tt, ff)                   => {
        annotate(scope, p)
        annotate(scope, tt)
        annotate(scope, ff)
      }
      case Invoke(ref, method, args, invs)   => {
        annotate(scope, ref)
        args.foreach(annotate(scope, _))
        invs.foreach(annotate(scope, _))
      }
      case x : Location => {
        annotate(scope, x)
      }
      case x : PrimitiveFunction =>
        annotate(scope, x)
      case Old(l) => annotate(scope, l)
      case BuiltInFunctionCall(ident, typeArgs, args) => {
        args.foreach(annotate(scope, _))
        typeArgs.foreach(_.foreach(annotate(scope, _)))
      }


    }
  }

  private def annotate(scope: SymTab, x: SyncOp): Unit = {
    x.scope = scope
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
      case IntType() =>
      case TidType() =>
      case VoidType() =>
      case BoolType() =>
      case MoverType() =>
      case BoogieType(name) =>
      case ClassType(ident) =>
      case x@ArrayType(enclosing, ident, thisAccess) =>
        annotate(scope, thisAccess)
      case CollectionType(name, typeArgs) =>
        typeArgs.foreach(annotate(scope, _))
      case TypeVar(_) =>
    }
  }

  private def annotate(scope: SymTab, x: PrimitiveFunction): Unit = {
    x.scope = scope
    x match {
      case Alloc(name, args, invs)   => {
        annotate(scope, name)
        args.foreach(annotate(scope, _))
        invs.foreach(annotate(scope, _))
      }
      case AAlloc(a, size)           => {
        annotate(scope, a)
        annotate(scope, size)
      }
      case Length(a)                 => {
        annotate(scope, a)
      }
      case Lock(a)                   => {
        annotate(scope, a)
      }
      case IsLocal(expr, tid)        => {
        annotate(scope, expr)
        annotate(scope, tid)
      }
      case IsShared(expr)            => {
        annotate(scope, expr)
      }
      case IsFresh(expr)            => {
        annotate(scope, expr)
      }
      case Holds(expr, tid)          => {
        annotate(scope, expr)
        annotate(scope, tid)
      }
      case NextCASSucceeds(loc, tid) => {
        annotate(scope, loc)
        annotate(scope, tid)
      }
      case Rand()                    => {}
      case NextSpecStep(_)           => {}
    }
  }

  def annotate(scope: SymTab, x: BuiltInFunctionDecl) : Unit = {
    x.scope = scope
    annotate(scope, x.returnType)
    x.parameters.foreach(annotate(scope, _))
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
  def annotate(scope: SymTab, x : MoverSpec, valueType: Type): Unit = {
    new BuildScope().annotate(scope, x, valueType)
  }
}
