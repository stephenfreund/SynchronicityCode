package anchor.sink

import anchor.util.Errors._
import PrettyPrint._
import anchor.transforms.Rename

import scala.util.parsing.input.NoPosition

class TypeCheck() {

  var currentMethod: MethodDecl = null
  var currentClass: ClassDecl = null

  sealed abstract class Mode
  case object Normal extends Mode
  case object InSpec extends Mode
  case object InTwoStatePredicate extends Mode

  private var mode : Mode = Normal

  def checkSpec[T](p : => T): T = {
    val t = this.mode
    this.mode = InSpec
    val result = p
    this.mode = t
    result
  }

  def checkTwoStatePredicate(p : => Unit): Unit = {
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
    for (f <- x.library.functions) {
      tc(f)
    }
    this.checkSpec {
      for (x <- x.axioms) {
        tc(x)
      }
    }
    for (x <- x.globals) {
      tc(x)
      x.t match {
        case BoolType()       =>
        case t@ClassType(name)  => check("Type", !t.decl.scope.methods.contains("init") || t.decl.scope.methods("init").params == Nil, s"Global object must have 0-ary constructor.", x)
        case ArrayType(_,_,_) => fail("Type", s"Global ${x.name} cannot be an array.", x)
        case _ => fail("Type", "Bad type for global", x)
      }
    }
    x.classes.foreach(tc(_))
  }

  private def tc(x: BuiltInFunctionDecl) : Unit = {
    for (p <- x.parameters) {
      tc(p)
    }
    tc(x.returnType)
  }

  private def fillInParents(x: ClassDecl): Unit = {
    for (f <- x.fields) {
      f.parent = x
    }
    for (m <- x.methods) {
      m.parent = x
    }
    for (a <- x.arrays) {
      a.parent = x
    }
  }

  private def tc(x: ClassDecl): Unit = {
    this.currentClass = x
    x.arrays.foreach(tc(_))
    x.fields.foreach(tc(_))
    x.methods.foreach(tc(_))
    checkSpec {
      x.invariants.foreach(tc(_))
    }
  }

  private def tc(x: ClassInvariant) : Unit = {
    this.checkSpec {
      x.triggers.foreach(_.foreach(tc(_)))
    }
    check("Type", tc(x.pred) == BoolType(), "Invarant must be Boolean expression")
  }

  private def tc(x: FieldModifier) : Unit = {
    x match {
      case VolatileModifier() =>
      case ABAFreeModifier()  =>
      case InternalModifier() =>
      case HasCASOperationModifier() =>
    }
  }

  private def tc(x: FieldDecl): Unit = {
    check("Type", Type.isValidFieldOrVarType(tc(x.t)), s"Bad type for field ${x.name}", x)
    tc(x.spec)
    for (m <- x.modifiers) {
      tc(m)
    }
  }

  private def tc(x: ArrayDecl): Unit = {
    check("Type", Type.isValidFieldOrVarType(tc(x.elemType)), s"Bad type for field ${x.name}", x)
    tc(x.spec)
  }
//
  private def tc(x: Spec): Unit = {
    checkSpec {
      check("Type", tc(x.conditionalMover) == MoverType(), s"Spec ${pp(x.conditionalMover)} must be a Mover expression", x)
      for (e <- x.yieldsAs) {
        check("Type", tc(e) == BoolType(), "Spec ensures clause must be Boolean expression", e)
      }
    }
  }

//  private def tc(x: Mover): Unit = {
//  }
//
//  private def tc(x: ConditionalMover, isRead : Boolean): Unit = {
//    x match {
//      case Base(m)       => tc(m)
//      case Cond(p, t, f) =>
//        val pType = tc(p)
//        check("Type", pType == BoolType(), s"Test must be a boolean expression, not ${p.t}", p)
//        p match {
//          case VarAccess("isRead") =>
//            tc(t, true)
//            tc(f, false)
//          case _                   =>
//            tc(t, isRead)
//            tc(f, isRead)
//        }
//    }
//  }

  private def checkReturns(cfg : ControlFlowGraph) : Unit = {
    // do dfs rather than just look at preds of exit because there may be dead code introduced
    // during Lowering...
    var visited : Set[BasicBlock] = Set()
    def dfs(current : BasicBlock) : Unit = {
      assert(current != cfg.exit)
      if (!visited.contains(current)) {
        if (!current.tac.isInstanceOf[ReturnTAC]) {
          val successors = current.getSuccessors()
          check("Type", !successors.contains(cfg.exit), "Missing Return", current.tac.stmt)
          visited = visited + current
          for (s <- successors) {
            dfs(s)
          }
        }
      }
    }
    dfs(cfg.enter)
  }

  private def tc(x: ExplicitMethodSpec): Unit = {
    for (require <- x.requires) {
      check("Type", tc(require) == BoolType(), "Requires clause must be boolean.", require)
    }
    for (v <- x.vars) {
      tc(v)
    }
    for (transaction <- x.transactions) {
      checkTwoStatePredicate {
        for (m <- transaction.modifies) {
          check("Type", isValidModifiesExpr(m), "Modifies must be location or old location.", m)
          tc(m)
        }
        for (e <- transaction.ensures) {
          check("Type", tc(e) == BoolType(), "Ensures clause must be boolean.", e)
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
  //  check("Type", !x.isCoop || x.name != "init", "init method cannot be cooperative", x)
    x.params.foreach(tc(_))
    val returnType = tc(x.returnType)
    check("Type", Type.isValidReturnType(returnType), s"Bad return type for field ${x.name}", x)
    currentMethod = x
    checkSpec {
      tc(x.spec)
    }
    tc(Nil, x.stmt)
    val lowered = new LowerAST().apply(x)
    val cfg = new ControlFlowGraph(lowered)
    checkReturns(cfg)
  }

  private def tc(x: VarDecl): Unit = {
    val xType = tc(x.t)
    check("Type", Type.isValidFieldOrVarType(xType, x.name == "this"), s"Bad type for variable ${x.name}", x)
  }

  private def resolveField(x: VarAccess, field: String): FieldDecl = {
    tc(x) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveField(field) match {
          case Some(f) => f
          case None => error("Type", s"Cannot find field ${name}.${field}", x); null
        }
      }
      case _ => error("Type", "designator is not an object", x); null
    }
  }

  private def resolveField(x: ClassType, field: String): FieldDecl = {
    tc(x) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveField(field) match {
          case Some(f) => f
          case None => error("Type", s"Cannot find field ${name}.${field}", x); null
        }
      }
      case _ => error("Type", "designator is not an object", x); null
    }
  }

  private def resolveInvoke(x: VarAccess, method: String): MethodDecl = {
    tc(x) match {
      case y@ClassType(name) => {
        y.decl.scope.resolveMethod(method) match {
          case Some(m) => m
          case None => error("Type", s"Cannot find method ${name}.${method}", x); null
        }
      }
      case _ => error("Type", "designator is not an object", x); null
    }
  }

  private def tc(locks: List[VarAccess], x: Stmt): Unit = {
    x match {
      case VarDeclStmt(v)                           => {
        tc(v)
      }
      case Assign(lhss, rhss)                         => {
        check("Type", lhss.size == rhss.size, "Assignment doesn't have same number of vars and exprs", x)
        for ((lhs, rhs) <- lhss.zip(rhss)) {
          val t1 = tc(lhs)
          val t2 = tc(rhs)
          checkNotLock(locks, lhs)
          check("Type", Type.isAssignmentConvertible(t2, t1), s"Bad types for assginment: ${pp(t1)} and ${pp(t2)}", x)
          check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
        }
      }
      case Block(name, body)                        => {
        body.foreach(tc(locks, _))
      }
      case x@Write(lhs, field, rhs, movesAs)        => {
        val f = resolveField(lhs, field)
        val t2 = tc(rhs)
        x.decl = f
        val renamedFieldType = renameThis(f.t, lhs)
        check("Type", Type.isAssignmentConvertible(t2, renamedFieldType), s"Bad types for write: field: ${pp(renamedFieldType)} and rhs:${pp(t2)}", x)
      }
      case LocalWrites(writes) => {
        writes.foreach(tc(locks, _))
      }

      case x@Read(lhs, rhs, field, movesAs)          => {
        val f = resolveField(rhs, field)
        val t2 = tc(lhs)
        assert(lhs.decl != null)
        val renamedFieldType = renameThis(f, rhs)
        checkNotLock(locks, lhs)
        check("Type", Type.isAssignmentConvertible(renamedFieldType, t2), s"Bad types for read: ${pp(t2)} and ${pp(renamedFieldType)}", x)
        check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
        x.decl = f
      }
      case x@Invoke(ref, method, args, res, invs)         => {
        x.decl = resolveInvoke(ref, method)
        check("Type", args.length == x.decl.params.length, "Wrong number of arguments", x)
        for (a <- args.zip(x.decl.params)) {
          check("Type", Type.isAssignmentConvertible(tc(a._1), a._2.t), s"Expected ${pp(a._2.t)} but got ${pp(a._1)}", a._1)
        }
        for (i <- invs) {
          checkSpec {
            check("Type", tc(i) == BoolType(), "Invariant must be a boolean expression", i)
          }
        }

        res match {
          case None    => {}
          case Some(v) => {
            val t = tc(v)
            tc(x.decl.returnType) // needed for forward refs??
            checkNotLock(locks, v)
            check("Type", Type.isAssignmentConvertible(x.decl.returnType, t), s"Bad types for call: ${pp(x.decl.returnType)} and ${pp(t)}", x)
            check("Type", !v.scope.isGlobal(v), "Cannot assign to global.", v)
          }
        }
      }

      case InlineInvoke(invoke) => {
        tc(locks, invoke)
      }
      case InlineReturn() => { }

      case Return(Some(e),_)                          => {
        check("Type", Type.isAssignmentConvertible(tc(e), currentMethod.returnType), "Bad return value", x)
      }
      case Return(None,_)                             => {
        check("Type", currentMethod.returnType == VoidType(), "Missing return value", x)
      }
      case Sync(lock, stmt, _)                      => {
        val lockType = tc(lock)
        check("Type", Type.isObject(lockType) || Type.isArray(lockType), "Lock must be an object/array", lock)
        tc(locks :+ lock, stmt)
      }
      case If(cond, t, f)                           => {
        val condType = tc(cond)
        check("Type", condType == BoolType(), s"Test must be a boolean expression, not ${cond.t}", cond)
        tc(locks, t)
        tc(locks, f)
      }
      case While(cond, stmt, invs, decreases)                  => {
        val contType = tc(cond)
        check("Type", contType == BoolType(), "Test must be a boolean expression", cond)
        for (i <- invs) {
          checkSpec {
            check("Type", tc(i) == BoolType(), "Invariant must be a boolean expression", i)
          }
        }
        for (e <- decreases) {
          checkSpec {
            check("Type", tc(e) == IntType(), "Decreasing expression must an integer expression", e)
          }
        }
        tc(locks, stmt)
      }
      case Break(label)                             => {}
      case x@CAS(result, lhs, field, expected, rhs) => {
        x.decl = resolveField(lhs, field)
        val expectedType = tc(expected)
        check("Type", Type.isAssignmentConvertible(expectedType, x.decl.t), s"Expected value has bad type: ${expected.t}", expected)
        val rhsType = tc(rhs)
        check("Type", Type.isAssignmentConvertible(rhsType, x.decl.t), s"New value has bad type: ${rhs.t}", rhs)
        val resultType = tc(result)
        check("Type", resultType == BoolType(), s"CAS result is boolean, not ${pp(result.t)}", result)
        check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
      }
      case Alloc(lhs, name)                         => {
        val lhsType = tc(lhs)
        val nameType = tc(name)
        checkNotLock(locks, lhs)
        check("Type", Type.isAssignmentConvertible(nameType, lhsType), "new object has wrong type: ${name.t}", name)
        check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
      }
      case Yield(ensures)                                  => {
        for (e <- ensures) {
          checkTwoStatePredicate {
            check ("Type", tc(e) == BoolType(), "Ensures must be a boolean expression", e)
          }
        }
      }
      case Commit() => { }
      case Assume(x)                                => {
        checkSpec {
          val xType = tc(x)
          check("Type", xType == BoolType(), s"Test must be a boolean expression, not ${x.t}", x)
        }
      }
      case Assert(x)                                => {
        val xType = tc(x)
        check("Type", xType == BoolType(), s"Test must be a boolean expression, not ${x.t}", x)
      }
      case Invariant(x)                                => {
        checkSpec {
          val xType = tc(x)
          check("Type", xType == BoolType(), s"Test must be a boolean expression, not ${x.t}", x)
        }
      }
      case x@AWrite(lhs, index, rhs)                => {
        val it = tc(index)
        check("Type", Type.isIntType(it), s"index be a integer", index)
        val lhsType = tc(lhs)
        check("Type", Type.isArray(lhsType), s"must have array, not ${lhs.t}", lhs)
        val elem = lhsType.asInstanceOf[ArrayType].elemType()
        val valueType = tc(rhs)
        check("Type", Type.isAssignmentConvertible(valueType, elem), s"Bad types for array write: ${pp(elem)} and ${pp(valueType)}", x)
      }
      case x@ARead(lhs, rhs, index)                 => {
        val indexType = tc(index)
        check("Type", Type.isIntType(indexType), s"index be a integer, not ${index.t}", index)
        val rhsType = tc(rhs)
        check("Type", Type.isArray(rhsType), s"must have array, not ${rhs.t}", rhs)
        val elem = rhsType.asInstanceOf[ArrayType].elemType()
        val valueType = tc(lhs)
        checkNotLock(locks, lhs)
        check("Type", Type.isAssignmentConvertible(elem, valueType), s"Bad types for array read: ${pp(rhs)} and ${pp(valueType)}", x)
        check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
      }
      case x@AAlloc(lhs, a, size)                   => {
        val lhsType = tc(lhs)
        val sizeType = tc(size)
        val arrayType = tc(a)
        checkNotLock(locks, lhs)
        check("Type", Type.isArray(arrayType), s"must have array, not ${pp(a)}", x)
        check("Type", sizeType == IntType(), s"must have int, not ${pp(size.t)}", x)
        check("Type", Type.isAssignmentConvertible(arrayType, lhsType), s"new array has wrong type: ${pp(arrayType)}, not ${pp(lhsType)}", x)
        check("Type", !lhs.scope.isGlobal(lhs), "Cannot assign to global.", lhs)
      }

      case BoogieCode(_)          =>
      case NoReductionCheck(stmt) => {
        tc(locks, stmt)
      }
      case Acquire(x)             => {
        val lockType = tc(x)
        check("Type", Type.isObject(lockType) || Type.isArray(lockType), "Lock must be an object/array", x)
      }
      case Release(x)             => {
        val lockType = tc(x)
        check("Type", Type.isObject(lockType) || Type.isArray(lockType), "Lock must be an object/array", x)
      }
    }
  }

  private def checkNotLock(locks: List[VarAccess], v: VarAccess) = {
    check("Type", locks.find(_.decl == v.decl) == None, s"Can't modify lock ${pp(v)} when held", v)
  }

  private def tc(x: Expr): Type = {
      x.t = x match {
        case ConstExpr(c) => {
          c match {
            case IntConst(v) => IntType()
            case BoolConst(v) => BoolType()
            case NullConst(t) => tc(t)
            case NullTid() => TidType()
            case MoverConst(m) => MoverType()
            case EmptyCollectionConst(t) => tc(t)
            case _ => assert(false); IntType()
          }
        }
        case BinaryExpr(lhs, rhs, op) => {
          (tc(lhs), op, tc(rhs)) match {

            case (IntType(), Add(), IntType()) => IntType()
            case (IntType(), Sub(), IntType()) => IntType()
            case (IntType(), Mul(), IntType()) => IntType()
            case (IntType(), Div(), IntType()) => IntType()
            case (IntType(), Mod(), IntType()) => IntType()

            case (BoolType(), Implies(), BoolType()) => BoolType()

            case (BoolType(), And(), BoolType()) => BoolType()
            case (BoolType(), Or(), BoolType()) => BoolType()


            case (IntType(), _:MathComparison, IntType())   => BoolType()
            case (TidType(), _:MathComparison, TidType())   => BoolType()
            case (IntType(), _:MathComparison, TidType())   => BoolType()
            case (TidType(), _:MathComparison, IntType())   => BoolType()

            case (x@ClassType(_), _:EqualComparison, y@ClassType(_)) if x.decl eq y.decl => BoolType()
            case (BoolType(), _:EqualComparison, BoolType()) => BoolType()
            case (MoverType(), _:EqualComparison, MoverType()) => BoolType()
            case (x@ArrayType(_,_,self1), _:EqualComparison, y@ArrayType(_,_, self2)) if (x.decl eq y.decl) && (self1 == self2) => BoolType()
              // TODO: Why names and not decls? must be copying decl somewhere...
            case (x@CollectionType(_,xa), _:EqualComparison, y@CollectionType(_,ya)) if (x.name == y.name) &&
                                                                                        xa.zip(ya).forall(z=>Type.isAssignmentConvertible(z._1,z._2)) => BoolType()

            case (l, o, r) =>
              error("Type", s"Bad binary expression: ${pp(x)}: ${pp(l)} and ${pp(r)} are not valid types", x)
              null
          }
        }
        case UnaryExpr(expr, op) => {
          (op, tc(expr)) match {
            case (Neg(), IntType())  => IntType()
            case (Not(), BoolType()) => BoolType()
            case (Paren(), t)        => t
            case (o, e)              => error("Type", s"Bad unary expression: ${o} ${e}", x); null
          }
        }

        case Cond(p, tt, ff) => {
          check("Type", tc(p) == BoolType(), "Condition value must be a boolean", p)
          val ttt = tc(tt)
          val fft = tc(ff)
          val ok = (ttt, fft) match {
            case (IntType(), TidType()) => true
            case (TidType(), IntType()) => true
            case (x,y) if x == y => true
            case _ => false
          }

          check("Type", ok, s"Branches of conditional do not match: ${pp(ttt)} and ${pp(fft)}", p)
          ttt
        }

        case Quantified(quantifier, decls, pred, triggers) => {
          check("Type", inSpec, "Forall can only appear in spec", x)
          decls.foreach(tc(_))
          val t = tc(pred)
          check("Type", t == BoolType(), "Predicate must be boolean expression", x)
          this.checkSpec {
            triggers.foreach(_.foreach(tc(_)))
          }
          t
        }
        case x: VarAccess => {
          val name = x.name
          x.scope.resolveVar(name) match {
            case Some(varDecl) =>
              x.decl = varDecl
              varDecl.t
            case None =>
              error("Type", s"Cannot find variable ${name}", x);
              IntType()
          }
        }
        case x@FieldAccess(v@VarAccess(_), field) => {
          check("Type", inSpec, "Can only access fields directly within specs", x)
          val t = tc(v)
          check("Type", Type.isObject(t), s"${pp(v)} is not an object", x)
          val f = resolveField(t.asInstanceOf[ClassType], field)
          x.decl = f
          renameThis(f.t, v)
        }

        case x@FieldAccess(v, field) => {
          check("Type", inSpec, "Can only access fields directly within specs", x)
          val t = tc(v)
          check("Type", Type.isObject(t), s"${pp(v)} is not an object", x)
          val f = resolveField(t.asInstanceOf[ClassType], field)
          x.decl = f
          check("Type", !Type.isArray(f.t), s"Can't rename this with location ${pp(v)}", x)
          f.t
        }

        case x@Old(l) => {
          check("Type", inTwoStatePredicate, "Can only use 'old' in two-state predicate")
          tc(l)
        }

        case x@ArrayAccess(a, index) => {
          check("Type", inSpec, "Can only access array elements directly within specs", x)
          val indexType = tc(index)
          check("Type", Type.isIntType(indexType), s"index be a integer", index)
          val arrayType = tc(a)
          check("Type", Type.isArray(arrayType), s"must have array type", a)
          val elem = arrayType.asInstanceOf[ArrayType].elemType()
          elem
        }

        case Length(a) => {
          val aType = tc(a)
          check("Type", Type.isArray(aType), "Can only take length of array", a)
          if (!inSpec) {
            check("Type", a.isInstanceOf[VarAccess], "can only pass variable to length function.", x)
          }

          assert(a.t != null)
          IntType()
        }
        case Holds(expr, tid)        => {
          check("Type", inSpec || expr.isInstanceOf[VarAccess], "Can only use `holds` on path expressions within specs", x)
          val exprType = tc(expr)
          check("Type", Type.isObject(exprType) || Type.isArray(exprType), "Holds value must be an object/array", expr)
          val tidType = tc(tid)
          check("Type", tidType == TidType(), "Tid value must be an int", tid)
          BoolType()
        }
        case Lock(expr)        => {
          check("Type", inSpec || expr.isInstanceOf[VarAccess], "Can only use `.lock` on path expressions within specs", x)
          val exprType = tc(expr)
          check("Type", Type.isObject(exprType) || Type.isArray(exprType), "Holds value must be an object/array", expr)
          TidType()
        }
        case IsLocal(expr, tid)      => {
          check("Type", inSpec || expr.isInstanceOf[VarAccess], "Can only use `isLocal` on path expressions within specs", x)
          val exprType = tc(expr)
          check("Type", Type.isObject(exprType) || Type.isArray(exprType), "IsLocal value must be an object/array", expr)
          val tidType = tc(tid)
          check("Type", tidType == TidType(), "Tid value must be an int", tid)
          BoolType()
        }
        case IsShared(expr)          => {
          check("Type", inSpec || expr.isInstanceOf[VarAccess], "Can only use `isShared` on path expressions within specs", x)
          val exprType = tc(expr)
          check("Type", Type.isObject(exprType) || Type.isArray(exprType), "IsShared value must be an object/array", expr)
          BoolType()
        }
        case IsFresh(expr)          => {
          check("Type", inSpec || expr.isInstanceOf[VarAccess], "Can only use `isFresh` on path expressions within specs", x)
          val exprType = tc(expr)
          check("Type", Type.isObject(exprType) || Type.isArray(exprType), "isFresh value must be an object/array", expr)
          BoolType()
        }
        case GoesWrong(e) => {
          val exprType = tc(e)
          check("Type", exprType == MoverType(), "GoesWrong must be applied to a mover value", e)
          BoolType()
        }
        case MoverPermission(l, v) => {
          check("Type", !l.isInstanceOf[VarAccess], "Can only use `permission` on path expresions")
          val exprType = checkSpec { tc(l) }
          v match {
            case Some(value) => {
              val valueType = tc(value)
              check("Type", Type.isAssignmentConvertible(valueType, exprType), s"Bad types for write: expected ${pp(exprType)}, not ${pp(valueType)}", x)
            }
            case None        => {

            }
          }
          MoverType()
        }
        case Rand()                  => {
          check("Type", !inSpec, "Can only use `rand` outside of specs", x)
          BoolType()
        }
        case NextSpecStep(n) => {
          BoolType()
        }

        case LoweredExpr(e, orig) => {
          val eType = tc(e)
          val oType = this.checkSpec { tc(orig) }
          check("Type", eType == oType, s"lowered type has different type! orig: ${orig} vs. lowered: ${e}", x)
          eType
        }

        case x@BuiltInFunctionCall(ident, typeArgs, args) => {
          x.decl = x.scope.resolveBuiltin(ident) match {
            case Some(value) => value
            case None        => fail("Type", s"Undefined builtin function: ${ident}", x)
          }
          check("Type", typeArgs.length == x.decl.typeVars.length, s"Wrong number of type arguments ${typeArgs.length} instead of ${x.decl.typeVars.length}", x)
          var substMap : Map[String, Type] = x.decl.typeVars.zip(typeArgs).toMap


          check("Type", args.length == x.decl.parameters.length, s"Wrong number of arguments to ${ident}", x)

          def unify(parameter: Type, arg: Type) : Boolean = {
            (parameter, arg) match {
              case (x, y) if x == y                       => true
              case (x, y) if Type.isIntType(x) && Type.isIntType(y) => true
              case (x: ClassType, y: ClassType)           => x.decl eq y.decl
              case (x: ArrayType, y: ArrayType)           => (x.decl eq y.decl) && x.thisAccess == y.thisAccess
              case (x: CollectionType, y: CollectionType) => {
                // TODO: Why is this == and not eq?  decls must be copied somewhere...
                (x.name == y.name) && x.typeArgs.zip(y.typeArgs).forall(a => unify(a._1, a._2))
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
            tc(t match {
              case TypeVar(n) if substMap.contains(n) => substMap(n)
              case TypeVar(n) => fail("Type", s"Unconstrained type variable ${n}", x)
              case t@CollectionType(kind, typeArgs) =>
                val r = AST.pos(CollectionType(kind, typeArgs.map(subst(_))), t.pos)
                BuildScope.annotate(t.scope, r)
                r
              case t => t
            })
          }

          for ((p:Type,a:Expr) <- x.decl.parameters.zip(args)) {
            val t = tc(a)
            check("Type", unify(p, t), s"Unification failure: ${pp(subst(p))} != ${pp(subst(t))}", a)
          }
          subst(x.decl.returnType)
        }


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


  private def renameThis(x: FieldDecl, newThis: VarAccess) : Type = {
    renameThis(x.t, newThis)
  }

  private def tc(x: Type): Type = {
    x match {
      case IntType()                              =>
      case TidType()                              =>
      case VoidType()                             =>
      case BoolType()                             =>
      case MoverType()                            =>
      case BoogieType(name)                       =>
      case x@ClassType(c)                         => {
        val scope = x.scope
        scope.resolveClass(x.name) match {
          case None       => error("Type", s"Class ${x.name} not found", x)
          case Some(decl) => x.decl = decl
        }
      }
      case x@ArrayType(enclosing, ident, thisVar) => {
        val c = x.scope.resolveClass(enclosing)
        check("Type", c != None, s"Class ${x.ident} not found", x)
        val scope = c.get.scope
        scope.resolveArray(x.ident) match {
          case None       => error("Type", s"Array ${x.ident} not found", x)
          case Some(decl) => x.decl = decl
        }
        thisVar.t = tc(thisVar)
        val t = tc(x.elemType())
        check("Type", t != VoidType(), "Can't have array of void elements", x)
        check("Type", !t.isInstanceOf[ArrayType], "Can't have array of arrays", x)
      }
      case x@CollectionType(name, args)           => {
        x.scope.resolveCollection(x.name) match {
          case None       => error("Type", s"Collection ${name} not found", x)
          case Some(decl) => x.decl = decl
        }
        check("Type", args.length == x.decl.numParams, s"Wrong number of type arguments: ${args.length} instead of ${x.decl.numParams}", x)

        for (arg <- args) {
          tc(arg)
        }
      }
      case TypeVar(name)                          =>

    }
    x
  }
}

object TypeCheck {
  def tc(x: Program): Unit = {
    new TypeCheck().tc(x)
  }

  def tc(x: Expr): Type = {
    new TypeCheck().tc(x)
  }

  def tc(x: Type): Type = {
    new TypeCheck().tc(x)
  }

  def tc(x: Spec): Unit = {
    new TypeCheck().tc(x)
  }

  def tc(scope: SymTab, x: Expr, inSpec: Boolean = false) : Type = {
    BuildScope.annotate(scope, x)
    val tc = new TypeCheck()
    if (inSpec) {
      tc.checkSpec {
        tc.tc(x)
      }
    } else {
      tc.tc(x)
    }
  }

  //  def tc(x: ConditionalMover) = {
//    new TypeCheck().tc(x)
//  }
}