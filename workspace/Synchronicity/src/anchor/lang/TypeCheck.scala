package anchor.lang

import anchor.util.Errors._
import PrettyPrint._
import anchor.transforms.FixPosition

class TypeCheck(val loweredExprs: Boolean = false) {

  var currentMethodReturnType: Type = null
  var currentClass: ClassDecl = null

  sealed abstract class Mode

  case object Normal extends Mode

  case object InSpec extends Mode

  case object InTwoStatePredicate extends Mode

  private var mode: Mode = Normal

  def checkSpec[T](p: => T): T = {
    val t = this.mode
    this.mode = InSpec
    val result = p
    this.mode = t
    result
  }

  def checkTwoStatePredicate(p: => Unit): Unit = {
    val t = this.mode
    this.mode = InTwoStatePredicate
    p
    this.mode = t
  }

  def inSpec = this.mode != Normal
  def inTwoStatePredicate = this.mode == InTwoStatePredicate


  def tc(x: Program): Unit = {
    for (c <- x.classes) {
      fillInParents(c)
    }
    for (f <- Library.functions) {
      tc(f)
    }

    for (x <- x.globals) {
      tc(x)
      val t = x.t
      t match {
        case BoolType()       =>
        case t@ClassType(name)  => check("Type", t.decl.constructor.params == Nil, s"Global object must have 0-ary constructor.",x)
        case ArrayType(_,_,_) => fail("Type", s"Global ${x.name} cannot be an array.", x)
        case _ => fail("Type", "Bad type for global", x)
      }
    }
    this.checkSpec {
      x.axioms.foreach(tc(_, Some(ExactExpectedType(BoolType(), "Axioms must be bool expressions"))))
    }
    x.classes.foreach(tc(_))
  }

  private def fillInParents(x: ClassDecl): Unit = {
    for (f <- x.fields) {
      f.parent = x
    }
    for (m <- x.methods) {
      m.parent = x
    }
    x.constructor.parent = x
    for (a <- x.arrays) {
      a.parent = x
    }
  }

  private def tc(x: ClassInvariant) : Unit = {
    this.checkSpec {
      x.triggers.foreach(_.foreach(tc(_, None)))
    }
    tc(x.pred, Some(new ExactExpectedType(BoolType(), "Invarant must be Boolean expression")))
  }

  private def tc(x: ClassDecl): Unit = {
    this.currentClass = x
    x.arrays.foreach(tc(_))
    x.fields.foreach(tc(_))
    x.methods.foreach(tc(_))
    tc(x.constructor)
    checkSpec {
      for (i <- x.invariants) {
        tc(i)
      }
    }
  }

  private def isVol(x: Expr): Boolean = {
    x match {
      case ConstExpr(c)    => {
        c match {
          case IntConst(v)     => false
          case BoolConst(v)    => false
          case NullConst()    => false
          case NullTid()       => false
          case MoverConst(B()) => false
          case MoverConst(E()) => false
          case MoverConst(_)   => true
          case _               => false
        }
      }
      case Cond(p, tt, ff) => isVol(tt) || isVol(ff)
      case _               => false
    }
  }


  private def tc(x: FieldDecl): Unit = {
    check("Type", Type.isValidFieldOrVarType(tc(x.t)), s"Bad type for field ${x.name}", x)
    tc(x.spec)
    check("Type", !isVol(x.spec.conditionalMover) || x.isVolatile, "Field with R/L/N in spec must be volatile.", x)
  }

  private def tc(x: ArrayDecl): Unit = {
    check("Type", Type.isValidFieldOrVarType(tc(x.elemType)), s"Bad type for field ${x.name}", x)
    tc(x.spec)
    check("Type", !isVol(x.spec.conditionalMover), "Array elem spec cannot have R/L/N.", x)
  }

  //
  private def tc(x: MoverSpec): Unit = {
    checkSpec {
      tc(x.conditionalMover, Some(new ExactExpectedType(MoverType(), s"Spec ${pp(x.conditionalMover)} must be a Mover expression")))
      for (e <- x.yieldsAs) {
        tc(e, Some(new ExactExpectedType(BoolType(), "Spec ensures clause must be Boolean expression")))
      }
    }
  }

  private def tc(x: ExplicitMethodSpec): Unit = {
    for (require <- x.requires) {
      tc(require, Some(ExactExpectedType(BoolType(), "Requires clause must be boolean.")))
    }
    for (v <- x.vars) {
      tc(v)
    }
    for (transaction <- x.transactions) {
        checkTwoStatePredicate {
          for (m <- transaction.modifies) {
            check("Type", isValidModifiesExpr(m), "Modifies must be location or old location.", m)
            tc(m, None)
          }
          for (e <- transaction.ensures) {
            tc(e, Some(ExactExpectedType(BoolType(), "Ensures clause must be boolean.")))
          }
      }
    }

  }

  private def isValidModifiesExpr(expr: Expr) = {
    expr match {
      case _: Location      => true
      case Old(_:Location)  => true
      case _ => false
    }
  }

  private def tc(x: MethodDecl): Unit = {
    x.params.foreach(tc(_))
    currentMethodReturnType = tc(x.returnType)
    check("Type", Type.isValidReturnType(currentMethodReturnType), s"Bad return type for method ${x.name}", x)
    if (x.name == x.parent.name) {
      check("Type", currentMethodReturnType == VoidType(), s"Bad return type for constructor", x)
    }
    checkSpec {
      tc(x.spec)
    }
    tc(x.stmt)
    // TODO: Check for returns here???
  }

  private def tc(x: ConstructorDecl): Unit = {
    x.params.foreach(tc(_))
    currentMethodReturnType = tc(VoidType())
    checkSpec {
      tc(x.spec)
    }
    tc(x.stmt)
    // TODO: Check for returns here???
  }

  private def tc(x: VarDecl): Unit = {
    val xType = tc(x.t)
    check("Type", Type.isValidFieldOrVarType(xType, x.name == "this"), s"Bad type for variable ${x.name}", x)
  }

  private def resolveField(x: VarAccess, field: String): FieldDecl = {
    tc(x, None) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveField(field) match {
          case Some(f) => f
          case None    => error("Type", s"Cannot find field ${name}.${field}", x); null
        }
      }
      case _                 => error("Type", "designator is not an object", x); null
    }
  }

  private def resolveField(x: Expr, field: String): FieldDecl = {
    tc(x, None) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveField(field) match {
          case Some(f) => f
          case None    => error("Type", s"Cannot find field ${name}.${field}", x); null
        }
      }
      case _                 => error("Type", "designator is not an object", x); null
    }
  }

  private def resolveInvoke(x: Expr, method: String): MethodDecl = {
    tc(x, None) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveMethod(method) match {
          case Some(m) => m
          case None    => error("Type", s"Cannot find method ${name}.${method}", x); null
        }
      }
      case _                 => error("Type", "designator is not an object", x); null
    }
  }

  private def tc(x: Stmt): Unit = {
    x match {
      case VarDeclStmt(v, e)                                  => {
        tc(v)
        e match {
          case Some(value) => {
            tc(value, Some(ExactExpectedType(v.t, "Bad types for assignment")))
          }
          case None        =>  {}
        }
      }
      case Assign(lhs, rhs)              => {
        val t1 = tc(lhs, None)
        val t2 = tc(rhs, Some(ExactExpectedType(t1, "Bad types for assignment")))
        lhs match {
          case x@VarAccess(name)       => check("Type", !x.scope.isGlobal(x), "Cannot assign to global.", x)
          case _ =>
        }
      }
      case LocalAssign(assigns)          => {
        assigns.foreach(tc(_))
      }
      case Block(name, body)             => {
        body.foreach(tc(_))
      }
      case Return(Some(e), _)            => {
        tc(e, Some(ExactExpectedType(currentMethodReturnType, "Bad return value")))
      }
      case Return(None, _)               => {
        check("Type", currentMethodReturnType == VoidType(), "Missing return value", x)
      }
      case ExprStmt(i)                   => {
        tc(i, None)
      }
      case SyncBlock(lock, stmt, _)      => {
        tc(lock, Some(ReferenceExpectedType("Lock must be an object/array")))
        tc(stmt)
      }
      case If(cond, t, f)                => {
        tc(cond, Some(ExactExpectedType(BoolType(), "Test must be a boolean expression")))
        tc(t)
        tc(f)
      }
      case While(cond, stmt, invs, decs) => {
        tc(cond, Some(ExactExpectedType(BoolType(), "Test must be a boolean expression")))
        for (i <- invs) {
          checkSpec {
            tc(i, Some(ExactExpectedType(BoolType(), "Invariant must be a boolean expression")))
          }
        }
        for (i <- decs) {
          checkSpec {
            tc(i, Some(ExactExpectedType(IntType(), "Decreasing expression must be an integer expression")))
          }
        }
        tc(stmt)
      }
      case Break(label)                                       => {}
      case Yield(ensures)                                     => {
        for (e <- ensures) {
          checkTwoStatePredicate {
            tc(e, Some(ExactExpectedType(BoolType(), "Ensures must be a boolean expression")))
          }
        }
      }
      case Commit() => {}
      case Assume(e)              => {
        checkSpec {
          tc(e, Some(ExactExpectedType(BoolType(), "Assume must be a boolean expression")))
        }
      }
      case Assert(e)              => {
        tc(e, Some(ExactExpectedType(BoolType(), "Assert must be a boolean expression")))
      }
      case Invariant(e)           => {
        checkSpec {
          tc(e, Some(ExactExpectedType(BoolType(), "Invariant must be a boolean expression")))
        }
      }
      case BoogieCode(_)          =>
      case NoReductionCheck(stmt) => {
        tc(stmt)
      }
      case SyncStmt(op, x)             => {
        val lockType = tc(x, None)
        check("Type", Type.isObject(lockType) || Type.isArray(lockType), "Lock must be an object/array", x)
      }
    }
  }

  private def checkNotLock(locks: List[VarAccess], v: VarAccess) = {
    check("Type", locks.find(_.decl == v.decl) == None, s"Can't modify lock ${pp(v)} when held", v)
  }

  sealed abstract class ExpectedType
  case class ExactExpectedType(val t: Type, val errorMessage: String) extends ExpectedType
  case class ReferenceExpectedType(val errorMessage: String) extends  ExpectedType
  case class ObjectExpectedType(val errorMessage: String) extends  ExpectedType
  case class ArrayExpectedType(val errorMessage: String) extends  ExpectedType

  private def tcPair(lhs: Expr, rhs: Expr, m : String): Type = {
    (lhs, rhs) match {
      case (ConstExpr(NullConst()), ConstExpr(NullConst())) => VoidType()
      case (ConstExpr(NullConst()), _) => tcPair(rhs, lhs, m)
      case (_, _) =>
    }
    tc(lhs, None) match {
      case IntType() | TidType() => tc(rhs, Some(ExactExpectedType(IntType(), m)))
      case BoolType()            => tc(rhs, Some(ExactExpectedType(BoolType(), m)))
      case MoverType()           => tc(rhs, Some(ExactExpectedType(MoverType(), m)))
      case x@ClassType(_)        => tc(rhs, Some(ExactExpectedType(x, m)))
      case x@ArrayType(_, _, _)  => tc(rhs, Some(ExactExpectedType(x, m)))
      case x@CollectionType(_, _) => tc(rhs, Some(ExactExpectedType(x, m)))
      case x                     => error("Type", s"Bad operand ${pp(x)}", lhs);
    }
    lhs.t
  }


  private def tc(x: Expr, expected: Option[ExpectedType]): Type = {
    x.t = x match {
      case ConstExpr(c)             => {
        c match {
          case IntConst(v)   => IntType()
          case BoolConst(v)  => BoolType()
          case c@NullConst()  => {
            expected match {
              case Some(ExactExpectedType(t, _)) => c.t = t; t
              case _                             => error("Type", "null appears in context without known type", x); null
            }
          }
          case NullTid()     => TidType()
          case MoverConst(m) => MoverType()
          case EmptyCollectionConst(t) => tc(t)
          case _             => error("Type", s"eh? ${c}"); IntType()
        }
      }
      case BinaryExpr(lhs, rhs, op) => {
        op match {
          case Add() | Sub() | Mul() | Div() | Mod() => {
            tc(lhs, Some(ExactExpectedType(IntType(), "Math operator expects integer type")))
            tc(rhs, Some(ExactExpectedType(IntType(), "Math operator expects integer type")))
            IntType()
          }
          case And() | Or() | Implies()              => {
            tc(lhs, Some(ExactExpectedType(BoolType(), "Boolean operator expects boolean type")))
            tc(rhs, Some(ExactExpectedType(BoolType(), "Boolean operator expects boolean type")))
            BoolType()
          }
          case LE() | LT() | GT() | GE()             => {
            tc(lhs, Some(ExactExpectedType(IntType(), "Math comparison expects integer type")))
            tc(rhs, Some(ExactExpectedType(IntType(), "Math comparison expects integer type")))
            BoolType()
          }

          case EQ() | NE() => {
            tcPair(lhs, rhs, "Equality operator types must match")
            BoolType()
          }
        }
      }


      case UnaryExpr(e, op)      => {
        op match {
          case Not()   => tc(e, Some(ExactExpectedType(BoolType(), "Boolean operator expects boolean type")))
          case Neg()   => tc(e, Some(ExactExpectedType(IntType(), "Math operator expects integer type")))
          case Paren() => tc(e, expected)
        }
      }

      case Cond(cond, tt, ff) => {
        tc(cond, Some(ExactExpectedType(BoolType(), "Test must be a boolean expression")))
        tcPair(tt, ff, "Conditional branch types must match")
      }

      case Quantified(_, decls, pred, triggers) => {
        decls.foreach(tc(_))
        check("Type", inSpec, "Forall can only appear in spec", x)
        tc(pred, Some(ExactExpectedType(BoolType(), "Predicate must be a boolean expression")))
        this.checkSpec {
          triggers.foreach(_.foreach(tc(_, None)))
        }
        BoolType()
      }
      case x: VarAccess                         => {
        val name = x.name
        x.scope.resolveVar(name) match {
          case Some(varDecl) =>
            x.decl = varDecl;
            varDecl.t
          case None          =>
            error("Type", s"Cannot find variable ${name}", x);
            IntType()
        }
      }

      case x@FieldAccess(lhs, field, isStable) => {
        val t = tc(lhs, Some(ObjectExpectedType("Designator is not an object")))
        val f = resolveField(lhs, field)
        x.decl = f
        x.t = lhs match {
          case x@VarAccess(name)                => this.renameThis(f.t, x)
          case _ if f.t.isInstanceOf[ArrayType] => error("Type", "If field has array type, designator expresion must be a local variable."); f.t
          case _                                => f.t
        }
        x.t
      }

      case x@CAS(lhs, field, expected, rhs) => {
        val t = tc(lhs, Some(ObjectExpectedType("Designator is not an object")))
        x.decl = resolveField(lhs, field)
        tc(expected, Some(ExactExpectedType(x.decl.t, s"Expected value has bad type")))
        tc(rhs, Some(ExactExpectedType(x.decl.t, s"New value has bad type")))
        BoolType()
      }


      case x@Old(l) => {
        check("Type", inTwoStatePredicate, "Can only use 'old' in two-state predicate")
        tc(l, None)
      }

      case x@ArrayAccess(a, index) => {
        tc(index, Some(ExactExpectedType(IntType(), s"Index must ben an integer")))
        tc(a, None) match {
          case ArrayType(_, _, _)            => {
            val arrayType = tc(a, Some(ArrayExpectedType("Designator must be an array")))
            arrayType.asInstanceOf[ArrayType].elemType()
          }
          case x@CollectionType("Seq", args) => {
            args(0)
          }
          case _                             => {
            fail("Type", s"Designator must be an array or Sequence", x)
          }
        }
      }

      case x@Invoke(ref, method, args, invs) => {
        check("Type", !inSpec, "Can only call methods outside of specs", x)

        x.decl = resolveInvoke(ref, method)
        check("Type", args.length == x.decl.params.length, "Wrong number of arguments", x)
        for (a <- args.zip(x.decl.params)) {
          tc(a._1, Some(ExactExpectedType(a._2.t, "Bad argument type")))
        }
        for (i <- invs) {
          checkSpec {
            tc(i, Some(ExactExpectedType(BoolType(), "Invariant must be a boolean expression")))
          }
        }

        val returnType = tc(x.decl.returnType) // needed for forward refs??
        returnType
      }
      case x@Alloc(c, args, invs)            => {
        check("Type", !inSpec, "Can only use `new` outside of specs", x)
        val t = tc(c)
        x.decl = c.decl.constructor
        for (a <- args.zip(x.decl.params)) {
          tc(a._1, Some(ExactExpectedType(a._2.t, "Bad argument type")))
        }
        for (i <- invs) {
          checkSpec {
            tc(i, Some(ExactExpectedType(BoolType(), "Invariant must be a boolean expression")))
          }
        }
        t
      }

      case x@AAlloc(a, size) => {
        check("Type", !inSpec, "Can only use `new` outside of specs", x)
        tc(size, Some(ExactExpectedType(IntType(), s"Size must be an integer")))
        val arrayType = tc(a)
        check("Type", Type.isArray(arrayType), s"must have array, not ${pp(a)}", x)
        arrayType
      }

      case Length(a) => {
        val t = tc(a, None) // , Some(ArrayExpectedType("Designator must be an array")))
        val test = t match {
          case a: ArrayType               => true
          case CollectionType("Seq", _)   => true
          case _ => false
        }
        check("Type", test , s"Expected: array or Seq, but got ${pp(x.t)}", x)
        IntType()
      }

      case Holds(expr, tid)        => {
        tc(expr, Some(ReferenceExpectedType("Expression must be an object/array")))
        tc(tid, Some(ExactExpectedType(IntType(), s"Tid must be an integer or tid value")))
        BoolType()
      }
      case Lock(expr)              => {
        tc(expr, Some(ReferenceExpectedType("Expression must be an object/array")))
        TidType()
      }
      case IsLocal(expr, tid)      => {
        tc(expr, Some(ReferenceExpectedType("Expression must be an object/array")))
        tc(tid, Some(ExactExpectedType(IntType(), s"Tid must be an integer or tid value")))
        BoolType()
      }
      case IsShared(expr)          => {
        tc(expr, Some(ReferenceExpectedType("Expression must be an object/array")))
        BoolType()
      }
      case IsFresh(expr)           => {
        tc(expr, Some(ReferenceExpectedType("Expression must be an object/array")))
        BoolType()
      }
      case NextCASSucceeds(l, tid) => {
        //check("Type", inSpec, "Can only use `casOK` within specs", x)
        val exprType = tc(l, None)
        check("Type", l.isInstanceOf[FieldAccess], "NextCASSucceeds location must be field access", l)
        tc(tid, Some(ExactExpectedType(IntType(), s"Tid must be an integer or tid value")))
        BoolType()
      }
      case Rand()                  => {
        check("Type", !inSpec, "Can only use `rand` outside of specs", x)
        BoolType()
      }
      case NextSpecStep(_)         => {
        BoolType()
      }
      case x@BuiltInFunctionCall(ident, typeArgs, args) => {
        x.decl = x.scope.resolveBuiltin(ident) match {
          case Some(value) => value
          case None        => fail("Type", s"Undefined builtin function: ${ident}", x)
        }
        var substMap = {
          typeArgs match {
            case Some(args) => {
              check("Type", args.size == x.decl.typeVars.length, s"Wrong number of type arguments: ${typeArgs.size} instead of ${x.decl.typeVars.size}", x)
              x.inferredTypes = args
              x.decl.typeVars.zip(args).toMap
            }
            case None        => Map[String,Type]()
          }
        }

        check("Type", args.length == x.decl.parameters.length, s"Wrong number of arguments to ${ident}", x)

        def unify(parameter: Type, arg: Type) : Boolean = {
          (parameter, arg) match {
            case (x, y) if x == y                       => true
            case (x, y) if Type.isIntType(x) && Type.isIntType(y) => true
            case (x: ClassType, y: ClassType)           => x.decl eq y.decl
            case (x: ArrayType, y: ArrayType)           => (x.decl eq y.decl) && x.thisAccess == y.thisAccess
            case (x: CollectionType, y: CollectionType) => {
              (x.decl eq y.decl) && x.typeArgs.zip(y.typeArgs).forall(a => unify(a._1, a._2))
            }
            case (TypeVar(v), y)                        => {
              substMap.get(v) match {
                case Some(resolved) => Type.isAssignmentConvertible(y, resolved)
                case None           => substMap = substMap + (v -> y); true
              }
            }
          }
        }

        def subst(t : Type) : Type = {
          t match {
            case TypeVar(n) if substMap.contains(n) => substMap(n)
            case TypeVar(n) => fail("Type", s"Unconstrained type variable ${n}", x)
            case t@CollectionType(name, typeArgs) => {
              val tt = AST.pos(CollectionType(name, typeArgs.map(subst(_))), t.pos)
              tt.decl = t.decl
              tt
            }
            case _ => t
          }
        }

        for ((p:Type,a:Expr) <- x.decl.parameters.zip(args)) {
          val t = tc(a, None)
          check("Type", unify(p, t), s"Unification failure: ${pp(p)} != ${pp(t)}: ${substMap.mapValues(pp(_))}", a)
        }
        if (x.inferredTypes == null) {
          x.inferredTypes = x.decl.typeVars.map(substMap(_))
        }
        subst(x.decl.returnType)
      }
    }
    assert(x.t != null)
    expected match {
      case Some(ExactExpectedType(t, m)) => check("Type", Type.isAssignmentConvertible(x.t, t), s"${m}. (Expected: ${pp(t)}, but got ${pp(x.t)})", x)
      case Some(ReferenceExpectedType(m)) => check("Type", Type.isHeapReference(x.t), s"${m}. (Expected: object/array, but got ${pp(x.t)})", x)
      case Some(ObjectExpectedType(m)) => check("Type", Type.isObject(x.t), s"${m}. (Expected: object, but got ${pp(x.t)})", x)
      case Some(ArrayExpectedType(m)) => check("Type", Type.isArray(x.t), s"${m}. (Expected: array, but got ${pp(x.t)})", x)
      case None                          =>
    }
    x.t
  }

  private def renameThis(x: Type, newThis: VarAccess): Type = {
    val t = x match {
      case x@ClassType(c)                                                                      => {
        val r = ClassType(c)
        r.decl = x.decl
        r
      }
      case x@ArrayType(enclosing, ident, thisVar) if thisVar.decl eq x.scope.resolveVar("this").get => {
        val r = ArrayType(enclosing, ident, newThis)
        r.decl = x.decl
        r
      }
      case x                                                                                   => x
    }

    BuildScope.annotate(newThis.scope, t)
    tc(t)
    t
  }

  private def tc(x: BuiltInFunctionDecl) : Unit = {
    for (p <- x.parameters) {
      tc(p)
    }
    tc(x.returnType)
  }

  private def tc(x: Type): Type = {
    x match {
      case IntType() =>
      case TidType() =>
      case VoidType() =>
      case BoolType() =>
      case MoverType() =>
      case BoogieType(name) =>
      case x@ClassType(c) => {
        val scope = x.scope
        scope.resolveClass(x.name) match {
          case None => error("Type", s"Class ${x.name} not found", x)
          case Some(decl) => x.decl = decl
        }
      }
      case x@ArrayType(enclosing, ident, thisVar) => {
        val c = x.scope.resolveClass(enclosing)
        check("Type", c != None, s"Class ${x.ident} not found", x)
        val scope = c.get.scope
        scope.resolveArray(x.ident) match {
          case None => error("Type", s"Array ${x.ident} not found", x)
          case Some(decl) => x.decl = decl
        }
        tc(thisVar, None) // ??
        val t = tc(x.elemType())
        check("Type", t != VoidType(), "Can't have array of void elements", x)
        check("Type", !t.isInstanceOf[ArrayType], "Can't have array of arrays", x)
      }
      case x@CollectionType(name,args) => {
        x.scope.resolveCollection(name) match {
          case None => fail("Type", "Cannot find library collection", x)
          case Some(y) => x.decl = y
        }
        check("Type", args.length == x.decl.numParams, "Wrong number of type arguments", x)

        for (arg <- args) {
          tc(arg)
        }
      }
      case TypeVar(name) =>
    }
    x
  }
}

object TypeCheck {
  def tc(x: Program): Unit = {
    new TypeCheck().tc(x)
  }

  def tc(x: Expr): Type = {
    new TypeCheck().tc(x, None)
  }

//  def tc(scope: SymTab, x: Expr, inSpec: Boolean = false): Type = {
//    BuildScope.annotate(scope, x)
//    val tc = new TypeCheck()
//    if (inSpec) {
//      tc.checkSpec {
//        tc.tc(x, None)
//      }
//    } else {
//      tc.tc(x, None)
//    }
//  }
}