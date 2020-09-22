package anchor.lang

import anchor.lang
import anchor.sink
import anchor.sink.ARead
import anchor.sink.AST.pos
import anchor.transforms.{FixPosition, Rename}
import anchor.util.Errors

import scala.util.parsing.input.{NoPosition, Position, Positional}

class AnchorToSink() {

  private var tmpNumber = 0

  def gen() = {
    tmpNumber += 1
    s"tmp${tmpNumber}"
  }

  def apply(x: Program): sink.Program = {
    pos(sink.Program(x.classes.map(this(_)),
                     x.globals.map(x => this(x)),
                     x.axioms.map(spec(_)),
                     new sink.Library(Library.collections.mapValues(this(_)))), x.pos)
  }

  def apply(x : Collection) : sink.Collection = {
    new sink.Collection(x.name, x.numParams, x.functions.map(this(_)), x.boogieText)
  }

  def apply(x: BuiltInFunctionDecl) : sink.BuiltInFunctionDecl = {
    pos(sink.BuiltInFunctionDecl(x.name, x.typeVars, x.parameters.map(this(_)), this(x.returnType)), x.pos)
  }

  def spec(x : ClassInvariant) : sink.ClassInvariant = {
    pos(sink.ClassInvariant(spec(x.pred), x.triggers.map(_.map(spec(_)))), x.pos)
  }


  def apply(x: ClassDecl): sink.ClassDecl = {
    pos(sink.ClassDecl(x.name, x.arrays.map(this (_)), x.fields.map(this (_)), this(x.constructor) :: x.methods.map(this (_)), x.invariants.map(spec(_))), x.pos)
  }

  def apply(x: ArrayDecl): sink.ArrayDecl = {
    pos(sink.ArrayDecl(x.name, this (x.elemType), x.elemName, this (x.spec)), x.pos)
  }

  def apply(x : FieldModifier) : List[sink.FieldModifier] = {
    x match {
      case VolatileModifier() => List(pos(sink.VolatileModifier(), x))
      case ABAFreeModifier()  => List(pos(sink.ABAFreeModifier(), x))
      case GhostModifier()    => Nil
    }
  }

  def apply(x: FieldDecl): sink.FieldDecl = {
    pos(sink.FieldDecl(this (x.t), x.name, this (x.spec), x.modifiers.map(this(_)).flatten), x.pos)
  }

  private def apply(x: MoverSpec): sink.Spec = {
    pos(sink.Spec(spec(x.conditionalMover), x.blocking, x.yieldsAs.map(spec(_))), x.pos)
  }

  def apply(x: VarAccess): sink.VarAccess = {
    pos(sink.VarAccess(x.name), x.pos)
  }

  def expandModifies(mod : Expr) : List[sink.Expr] = {
    if (mod == VarAccess("this")) {
      for (f <- mod.t.asInstanceOf[ClassType].decl.fields;
           if (Type.isArray(f.t)))
        yield pos(sink.FieldAccess(sink.VarAccess("this"), f.name), mod)
    } else {
      Nil
    }
  }

  def apply(x: Transaction) : sink.Transaction = {
    pos(sink.Transaction(x.repeats,
      x.modifies.map(spec(_)) ++ x.modifies.flatMap(expandModifies(_)),
      x.ensures.map(spec(_))), x.pos)
  }

  def apply(x: ExplicitMethodSpec) : sink.ExplicitMethodSpec = {
    pos(sink.ExplicitMethodSpec(
      x.requires.map(spec(_)),
      x.vars.map(this(_)),
      x.transactions.map(this(_))), x)
  }

  def apply(x: MethodDecl): sink.MethodDecl = {
    val xspec =  this(x.spec)
    val newSpec = pos(sink.ExplicitMethodSpec(xspec.requires, xspec.vars, xspec.transactions), xspec.pos)
    pos(sink.MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), newSpec, pos(blockIt(this (x.stmt)), x.stmt)),
      x.pos)
  }

  def apply(x: ConstructorDecl): sink.MethodDecl = {
    val extraRequires : List[sink.Expr] = if (x.isPublic) {
      for (f <- x.parent.fields if Type.isHeapReference(f.t)) yield {
        val nullConst = NullConst()
        nullConst.t = f.t
        FixPosition(x.pos)(spec(BinaryExpr(FieldAccess(VarAccess("this"), f.name), ConstExpr(nullConst), EQ())))
      }
    } else {
      List.empty
    }

    val defaultValues : List[sink.Stmt] = {
      def defaultValue(x: Type): Const = {
        x match {
          case IntType()            => IntConst(0)
          case BoolType()           => BoolConst(false)
          case x@ClassType(_)       => {
            val c = NullConst()
            c.t = x
            c
          }
          case x@ArrayType(_, _, _) => {
            val c = NullConst()
            c.t = x
            c
          }
          case TidType()            => NullTid()
          case x@CollectionType(_, _) => EmptyCollectionConst(x)
          case _                    => assert(false); IntConst(0)
        }
      }

      (for (f <- x.parent.fields) yield {
        this(Assume(BinaryExpr(FieldAccess(VarAccess("this"), f.name), ConstExpr(defaultValue(f.t)), EQ()))).map(FixPosition(x.pos)(_))
      }).flatten
    }

    val xspec =  this(x.spec)
    val newSpec = pos(sink.ExplicitMethodSpec(extraRequires ++ xspec.requires, xspec.vars, xspec.transactions), xspec.pos)
    pos(sink.MethodDecl(x.isPublic, this (pos(VoidType(), x.pos)), "init", x.params.map(this (_)), newSpec, pos(blockIt(defaultValues ++  this (x.stmt)), x.stmt)),
      x.pos)
  }

  private def blockIt(x: List[sink.Stmt]): sink.Stmt = {
    x match {
      case single :: Nil => single
      case stmts         => sink.Block(None, stmts)
    }
  }

  def apply(x: VarDecl): sink.VarDecl = {
    pos(sink.VarDecl(this (x.t), x.name), x.pos)
  }

  def init(x: VarDeclStmt): List[sink.Stmt] = {
    x.rhs match {
      case Some(rhs) => compute(rhs, Some(access(this(x.decl), x.pos)))._1
      case None        => Nil
    }
  }

  def tc(v : VarAccess) = {
    v.t = v.decl.t
    v
  }

  def apply(x: Stmt): List[sink.Stmt] = {
    pos(
      x match {
        case VarDeclStmt(decl, None)              => {
          List(sink.VarDeclStmt(this (decl)))
        }
        case VarDeclStmt(decl, Some(rhs))         => {
          val d = this (decl)
          sink.VarDeclStmt(d) :: compute(rhs, Some(access(d, x.pos)))._1 // can ignore result loc here since we know it...
        }
        case Assign(v: VarAccess, rhs)            => {
          val v2 = sink.VarAccess(v.name)
          v2.t = this(v.t)
          compute(rhs, Some(pos(v2, v)))._1 // can ignore result loc here since we know it...
        }
        case Assign(y@FieldAccess(obj, field, true), rhs) => {
          val (objCode, objResult) = compute(obj, None)
          val (rhsCode, rhsResult) = compute(rhs, None)
          val wr = sink.Write(objResult, field, rhsResult)
          val wrappedWr = if (y.decl.isGhost) {
            sink.NoReductionCheck(pos(wr, x.pos))
          } else {
            wr
          }
          (objCode ++ rhsCode) :+ wrappedWr
        }
        case Assign(FieldAccess(obj, field, false), rhs) => {
          Errors.fail("AnchorToString", "Can't have unstable write", x)
        }
        case Assign(ArrayAccess(arr, index), rhs) => {
          val (arrCode, arrResult) = compute(arr, None)
          val (indexCode, indexResult) = compute(index, None)
          val (rhsCode, rhsResult) = compute(rhs, None)
          (arrCode ++ indexCode ++ rhsCode) :+ sink.AWrite(arrResult, indexResult, rhsResult)
        }
        case LocalAssign(assigns) => {
          val writes = for (a <- assigns) yield {
            a match {
              case x@Assign(fa@FieldAccess(vo@VarAccess(obj), field, true), vr@VarAccess(rhs)) => {
                pos(sink.Write(pos(sink.VarAccess(obj), vo.pos), field, pos(sink.VarAccess(rhs), vr.pos)), x.pos)
              }
              case _                                                                     => {
                Errors.fail("AnchorToString", "Can only have local assigns of the form var.f = var", a.pos)
              }
            }
          }
          List(sink.LocalWrites(writes))
        }
        case Block(label, body)                   => {
          List[sink.Stmt](sink.Block(label, body.map(x => this (x)).flatten))
        }
        // the only expression that computes void type
        case ExprStmt(m@Invoke(ref, method, args, invs)) if (m.decl.returnType == VoidType()) => {
          val (refCode, refResult) = compute(ref)
          val (stmts, argVars) = args.map(computeToVarOrConst(_, None)).unzip
          (refCode ++ stmts.flatten) :+ pos(sink.Invoke(refResult, method, argVars, None, invs.map(spec(_))), m)
        }
        case ExprStmt(e)                                                                      => {
          compute(e)._1 // ignore result value
        }
        case Return(None, isSynthetic)                                                        => List(sink.Return(None, isSynthetic))
        case Return(Some(e), isSynthetic)                                                     => {
          val (code, result) = computeToVarOrConst(e)
          code :+ sink.Return(Some(result), isSynthetic)
        }
        case If(e, trueBranch, falseBranch)                                                   => {
          val (code, result) = compute(e)
          code :+ sink.If(recordLowered(result, e),
            pos(blockIt(this (trueBranch)), trueBranch),
            pos(blockIt(this (falseBranch)), falseBranch))
        }
        case While(e, stmt, invariants, decreases)                                            => {
          val (code, result) = compute(e)
          val ifStmt = pos(sink.If(pos(sink.AST.not(result), x), pos(sink.Break(None), x), pos(sink.Block(None, Nil), x)), x)
          sink.While(pos(sink.ConstExpr(pos(sink.BoolConst(true), x)), x),
            pos(blockIt(code ++ (ifStmt :: this (stmt))), stmt.pos),
            invariants.map(spec(_)),
            decreases.map(spec(_))
          ) :: Nil


        }
        case Break(label)                                                                     => sink.Break(label) :: Nil
        case Assume(expr)                                                                     => sink.Assume(spec(expr)) :: Nil
        case Assert(b@BinaryExpr(lhs, rhs, EQ()))                                                                     => {
          // asserts have run-time behavior -- must *compute* expr here
          val (lcode, lhss) = computeToVarOrConst(lhs)
          val (rcode, rhss) = computeToVarOrConst(rhs)
          (lcode ++ rcode) :+ sink.Assert(pos(sink.BinaryExpr(lhss, rhss, sink.EQ()), b.pos))
        }
        case Assert(expr)                                                                     => {
          // asserts have run-time behavior -- must *compute* expr here
          val (code, result) = computeToVarOrConst(expr)
          code :+ sink.Assert(recordLowered(result, expr))
        }
        case Invariant(expr)                                                                  => sink.Invariant(spec(expr)) :: Nil
        case Yield(ensures)                 => sink.Yield(ensures.map(spec(_))) :: Nil
        case Commit()                       => sink.Commit() :: Nil
        case BoogieCode(code)               => sink.BoogieCode(code) :: Nil
        case NoReductionCheck(stmt)         => sink.NoReductionCheck(pos(sink.Block(None, this (stmt)), x.pos)) :: Nil
        case SyncBlock(e, stmt, releasePos) => {
          val (code, result) = compute(e)
          code :+ sink.Sync(result, pos(blockIt(this (stmt)), stmt), releasePos)
        }
        case SyncStmt(Acquire(), e)                     => {
          val (code, result) = compute(e)
          code :+ sink.Acquire(result)
        }
        case SyncStmt(Release(), e)                   => {
          val (code, result) = compute(e)
          code :+ sink.Release(result)
        }
        case SyncStmt(Notify(), e) => {
          Nil
        }
        case SyncStmt(Wait(), e) => {
          val (code, result) = compute(e)
          code ++ List(pos(sink.Release(result), x), pos(sink.Yield(Nil), x), pos(sink.Acquire(result), x))
        }
      }, x.pos)
  }


  private def recordLowered(lowered: sink.VarOrConst, original: Expr) = {
    pos(sink.LoweredExpr(lowered, Errors.possiblyFail(spec(original))), original.pos)
  }

  def apply(x: Mover): sink.Mover = {
    x match {
      case I() => sink.I()
      case B() => sink.B()
      case R() => sink.R()
      case L() => sink.L()
      case N() => sink.N()
      case E() => sink.E()
    }
  }

  def apply(x: BinaryOp): sink.BinaryOp = {
    pos(x match {
      case Add()     => sink.Add()
      case Sub()     => sink.Sub()
      case Mul()     => sink.Mul()
      case Div()     => sink.Div()
      case Mod()     => sink.Mod()
      case And()     => sink.And()
      case Or()      => sink.Or()
      case EQ()      => sink.EQ()
      case NE()      => sink.NE()
      case LT()      => sink.LT()
      case GT()      => sink.GT()
      case LE()      => sink.LE()
      case GE()      => sink.GE()
      case Implies() => sink.Implies()
    },x.pos)
  }

  def apply(x: UnaryOp): sink.UnaryOp = {
    pos(x match {
      case Not() => sink.Not()
      case Neg() => sink.Neg()
      case Paren() => sink.Paren()
    }, x.pos)
  }

  def apply(x: Const): sink.Const = {
    pos(x match {
      case IntConst(v)   => sink.IntConst(v)
      case BoolConst(v)  => sink.BoolConst(v)
      case x@NullConst()  => sink.NullConst(this (x.t).asInstanceOf[sink.RefType])
      case NullTid()     => sink.NullTid()
      case MoverConst(m) => sink.MoverConst(this (m))
      case EmptyCollectionConst(t) => sink.EmptyCollectionConst(this(t).asInstanceOf[sink.CollectionType])
    }, x.pos)
  }

  def makeTemp(t: Type, p : Position): (List[sink.Stmt], sink.VarDecl) = {
    val decl = pos(sink.VarDecl(pos(this (t), p), gen()), p)
    (List(pos(sink.VarDeclStmt(decl), p)), decl)
  }

  def makeTemp(t: sink.Type, p : Position): (List[sink.Stmt], sink.VarDecl) = {
    val decl = pos(sink.VarDecl(pos(t, p), gen()), p)
    (List(pos(sink.VarDeclStmt(decl), p)), decl)
  }


  def access(decl: sink.VarDecl, p : Position) = {
    val a = pos(new sink.VarAccess(decl), p)
    a.t = decl.t
    assert(a.decl != null)
    a
  }


  private def resultFromOption(result: Option[sink.VarAccess], t: Type, p : Positional) = {
    val (stmt, acc) = result match {
      case Some(value) => //assert(value.t == t);
        (Nil, value)
      case None        => {
        val (stmt, decl) = makeTemp(t, p.pos)
        (stmt, access(decl, p.pos))
      }
    }
    (stmt, acc)
  }


  def tc(x : sink.VarAccess) = {
    x
  }

  def copy(x : sink.VarAccess) = {
    val c = x.copy()
    tc(c)
  }

  def compute(x: Location, result: Option[sink.VarAccess]): (List[sink.Stmt], sink.VarAccess) = {
    x match {
      case x@VarAccess(l)         if result == None => (Nil, pos(sink.VarAccess(l), x))
      case x@VarAccess(l)          => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        (stmt :+ pos(sink.Assign(res, pos(sink.VarAccess(l), x)), x.pos), res)
      }
      case x@FieldAccess(v, field, isStable) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (code, tmp) = compute(v)
        if (isStable) {
          val rd = pos(sink.Read(res, tmp, field), x)
          val rd2 = if (x.decl.isGhost) {
            sink.NoReductionCheck(pos( rd , x.pos))
          } else {
            pos(rd, x)
          }
          (stmt ++ code :+ rd2, pos(copy(res), x))
        } else {
          val randomVar = this.makeTemp(x.t, x.pos)
          val nullCheck = FixPosition(x.pos)(sink.Assert(sink.BinaryExpr(tmp.copy(), sink.ConstExpr(sink.NullConst(this(v.t).asInstanceOf[sink.RefType])), sink.NE())))
          (randomVar._1 ++ stmt ++ code :+ nullCheck :+ pos(sink.Assign(res, access(randomVar._2, x.pos)), x) , pos(copy(res), x))
        }
      }
      case x@ArrayAccess(l, index) => {
        val (arrayCode, arrayName) = compute(l, None)
        val (indexCode, indexName) = compute(index)
        val (resultCode, res) = resultFromOption(result, x.t, x)
        val op = l.t match {
          case ArrayType(_, _, _)            => {
            sink.ARead(pos(copy(res), x), arrayName, indexName)
          }
          case x@CollectionType("Seq", args) => {
            sink.BuiltInFunctionCall("SeqNth", args.map(this (_)), List(arrayName, indexName))
          }
          case _                             => {
            Errors.fail("Type", s"Designator must be an array or Sequence", x)
          }
        }

        ((arrayCode ++ indexCode ++ resultCode) :+ pos(sink.ARead(pos(copy(res), x), arrayName, indexName), x), pos(copy(res), x))
      }
    }
  }

  def computeToVarOrConst(x: Expr, result: Option[sink.VarAccess] = None): (List[sink.Stmt], sink.VarOrConst) = {
    x match {
      case x@ConstExpr(c) => {
        (Nil, pos(sink.ConstExpr(this (c)), x.pos))
      }
      case BinaryExpr(lhs, rhs, Or())                 => {
        val t = ConstExpr(BoolConst(true))
        t.t = BoolType()
        val c = Cond(lhs, t, rhs)
        c.t = BoolType()
        computeToVarOrConst(c, result)
      }
      case BinaryExpr(lhs, rhs, And())                 => {
        val f = ConstExpr(BoolConst(false))
        f.t = BoolType()
        val c = Cond(lhs, rhs, f)
        c.t = BoolType()
        computeToVarOrConst(c, result)
      }

      case BinaryExpr(lhs, rhs, op)                 => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (lhsCode, lhsResult) = computeToVarOrConst(lhs)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        ((stmt ++ lhsCode ++ rhsCode) :+
          pos(sink.Assign(pos(copy(res), x), pos(sink.BinaryExpr(lhsResult, rhsResult, this (op)), x)), x), pos(copy(res), x))
      }
      case UnaryExpr(rhs, op) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        ((stmt ++ rhsCode) :+
          pos(sink.Assign(pos(copy(res), x), pos(sink.UnaryExpr(rhsResult, this (op)), x)), x), pos(copy(res), x))
      }
      case x@Invoke(ref, method, args, invs) => {

        assert(x.decl.returnType != VoidType())
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (refCode, refResult) = compute(ref)
        val (stmts, argVars) = args.map(computeToVarOrConst(_, None)).unzip
        ((stmt ++ refCode ++ stmts.flatten) :+ pos(sink.Invoke(refResult, method, argVars, Some(pos(copy(res), x)), invs.map(spec(_))), x), pos(copy(res), x))
      }
      case CAS(ref, field, expected, rhs)                 => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (refCode, refResult) = compute(ref)
        val (expCode, expResult) = computeToVarOrConst(expected)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        (stmt ++ refCode ++ expCode ++ rhsCode :+ pos(sink.CAS(pos(copy(res), x), refResult, field, expResult, rhsResult), x), pos(copy(res), x))
      }

      case x@Cond(test, tt, ff)              => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (testCode, testResult) = computeToVarOrConst(test)
        val (trueCode, _) = compute(tt, Some(pos(tc(copy(res)), res)))
        val (falseCode, _) = compute(ff, Some(pos(tc(copy(res)), res)))
        ((stmt ++ testCode) :+
          pos(sink.If(testResult,
            pos(sink.Block(None, trueCode), x),
            pos(sink.Block(None, falseCode), x)), x), pos(tc(copy(res)), x))

      }
      case Quantified(_,decls, pred, triggers) => {
        Errors.fail("AnchorToSink", "Cannot have ForAll in computable statements")
      }
      case p: Location                       => compute(p, result)
      case x: PrimitiveFunction              => compute(x, result)
      case Old(l)                  => {
        Errors.fail("AnchorToSink", "Cannot have Old in computable statements")
      }
      case x@BuiltInFunctionCall(name, types, arguments) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (stmts, vars) = arguments.map(computeToVarOrConst(_)).unzip
        ((stmt ++ stmts.flatten) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.BuiltInFunctionCall(name, x.inferredTypes.map(this(_)), vars), x)), x), res)
      }
    }
  }

  def compute(x: Expr, result: Option[sink.VarAccess] = None): (List[sink.Stmt], sink.VarAccess) = {
    x match {
      case x@ConstExpr(c) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        (stmt :+ pos(sink.Assign(pos(copy(res), x), pos(sink.ConstExpr(this (c)), x.pos)), x), pos(copy(res), x))
      }
      case BinaryExpr(lhs, rhs, Or())                 => {
        val t = ConstExpr(BoolConst(true))
        t.t = BoolType()
        val c = Cond(lhs, t, rhs)
        c.t = BoolType()
        compute(c, result)
      }
      case BinaryExpr(lhs, rhs, And())                 => {
        val f = ConstExpr(BoolConst(false))
        f.t = BoolType()
        val c = Cond(lhs, rhs, f)
        c.t = BoolType()
        compute(c, result)
      }

      case BinaryExpr(lhs, rhs, op)                 => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (lhsCode, lhsResult) = computeToVarOrConst(lhs)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        ((stmt ++ lhsCode ++ rhsCode) :+
          pos(sink.Assign(pos(copy(res), x), pos(sink.BinaryExpr(lhsResult, rhsResult, this (op)), x)), x), pos(copy(res), x))
      }
      case UnaryExpr(rhs, op) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        ((stmt ++ rhsCode) :+
          pos(sink.Assign(pos(copy(res), x), pos(sink.UnaryExpr(rhsResult, this (op)), x)), x), pos(copy(res), x))
      }
      case x@Invoke(ref, method, args, invs) => {

        assert(x.decl.returnType != VoidType())
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (refCode, refResult) = compute(ref)
        val (stmts, argVars) = args.map(computeToVarOrConst(_, None)).unzip
        ((stmt ++ refCode ++ stmts.flatten) :+ pos(sink.Invoke(refResult, method, argVars, Some(pos(copy(res), x)), invs.map(spec(_))), x), pos(copy(res), x))
      }
      case CAS(ref, field, expected, rhs)                 => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (refCode, refResult) = compute(ref)
        val (expCode, expResult) = computeToVarOrConst(expected)
        val (rhsCode, rhsResult) = computeToVarOrConst(rhs)
        (stmt ++ refCode ++ expCode ++ rhsCode :+ pos(sink.CAS(pos(copy(res), x), refResult, field, expResult, rhsResult), x), pos(copy(res), x))
      }

      case x@Cond(test, tt, ff)              => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (testCode, testResult) = computeToVarOrConst(test)
        val (trueCode, _) = computeToVarOrConst(tt, Some(pos(tc(copy(res)), res)))
        val (falseCode, _) = computeToVarOrConst(ff, Some(pos(tc(copy(res)), res)))
        ((stmt ++ testCode) :+
          pos(sink.If(testResult,
            pos(sink.Block(None, trueCode), x),
            pos(sink.Block(None, falseCode), x)), x), pos(tc(copy(res)), x))

      }
      case Quantified(_,decls, pred, triggers) => {
        Errors.fail("AnchorToSink", "Cannot have ForAll in computable statements")
      }
      case p: Location                       => compute(p, result)
      case x: PrimitiveFunction              => compute(x, result)
      case Old(l)                  => {
        Errors.fail("AnchorToSink", "Cannot have Old in computable statements")
      }
      case x@BuiltInFunctionCall(name, types, arguments) => {
        val (stmt, res) = resultFromOption(result, x.t, x)
        val (stmts, vars) = arguments.map(computeToVarOrConst(_)).unzip
        ((stmt ++ stmts.flatten) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.BuiltInFunctionCall(name, x.inferredTypes.map(this(_)), vars), x)), x), res)
      }

    }
  }

  private def compute(x: PrimitiveFunction, result: Option[sink.VarAccess]): (List[sink.Stmt], sink.VarAccess) = {
    val (stmt, res) = resultFromOption(result, x.t, x)

    x match {
      case x@Length(a)             => {
        val (argCode, argResult) = compute(a)
        val op = a.t match {
            case ArrayType(_,_,_)               => sink.Length(argResult)
            case x@CollectionType("Seq", args)   => sink.BuiltInFunctionCall("SeqLen", args.map(this(_)), List(argResult))
            case _ => assert(false);         sink.Length(argResult)
          }

        ((stmt ++ argCode) :+ pos(sink.Assign(pos(copy(res), x), pos(op, x)), x), pos(copy(res), x))
      }
      case x@Lock(a)             => {
        val (argCode, argResult) = compute(a)
        ((stmt ++ argCode) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.Lock(argResult), x)), x), pos(copy(res), x))
      }
      case x@Alloc(name, args, invs)           => {
        val (stmts, argVars) = args.map(computeToVarOrConst(_, None)).unzip

        (stmt ++
          List(pos(sink.Alloc(pos(copy(res),x), this (name).asInstanceOf[sink.ClassType]),x)) ++
          stmts.flatten ++
          List(pos(sink.Invoke(pos(copy(res), x), "init", argVars, None, invs.map(spec(_))),x)),
         pos(copy(res), x))
      }
      case x@AAlloc(a, size)       => {
        val (sizeCode, sizeResult) = computeToVarOrConst(size)
        ((stmt ++ sizeCode) :+ pos(sink.AAlloc(pos(copy(res), x), this (a).asInstanceOf[sink.ArrayType], sizeResult), x), pos(copy(res), x))
      }
      case x@Holds(e, t)           => {
        val (argCode, argResult) = compute(e)
        val (tCode, tResult) = compute(t)
        ((stmt ++ argCode ++ tCode) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.Holds(argResult, tResult), x)), x), pos(copy(res), x))

//        Errors.fail("AnchorToSink", "Cannot have `holds` in computable statements")
      }
      case x@IsLocal(a, t)         => {
        Errors.fail("AnchorToSink", "Can not have `isLocal` in computable statements")
      }
      case x@IsShared(a)           => {
        val (argCode, argResult) = compute(a)
        ((stmt ++ argCode) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.IsShared(argResult), x)), x), pos(copy(res), x))
      }
      case x@IsFresh(a)           => {
        val (argCode, argResult) = compute(a)
        ((stmt ++ argCode) :+ pos(sink.Assign(pos(copy(res), x), pos(sink.IsFresh(argResult), x)), x), pos(copy(res), x))
      }
      case x@NextCASSucceeds(e, t) => {
        Errors.fail("AnchorToSink", "Cannot have `casOK` in computable statements")
      }
      case Rand()                  => {
        (stmt :+ sink.Assign(pos(copy(res), x), pos(sink.Rand(), x)), pos(copy(res), x))
      }
      case NextSpecStep(n) => {
        (stmt :+ sink.Assign(pos(copy(res), x), pos(sink.NextSpecStep(n), x)), pos(copy(res), x))
      }
    }
  }


  def spec(x: Location): sink.Expr = {
    pos(x match {
      case x@VarAccess(l)          => sink.VarAccess(l)
      case x@FieldAccess(v, field, true) => sink.FieldAccess(spec(v), field)
      case x@FieldAccess(v, field, false) => Errors.fail("AnchorToSink", "Cannot have unstable read in spec", x)
      case x@ArrayAccess(l, index) => {
        l.t match {
          case ArrayType(_, _, _)            => {
            sink.ArrayAccess(spec(l), spec(index))
          }
          case x@CollectionType("Seq", args) => {
            sink.BuiltInFunctionCall("SeqNth", args.map(this(_)), List(spec(l), spec(index)))
          }
          case _                             => {
            Errors.fail("Type", s"Designator must be an array or Sequence", x)
          }
        }

      }
    }, x.pos)
  }


  def spec(x: Expr): sink.Expr = {
    pos(x match {
      case x@ConstExpr(c)                    => {
        pos(sink.ConstExpr(this (c)), x.pos)
      }
      case BinaryExpr(lhs, rhs, op)          => {
        sink.BinaryExpr(spec(lhs), spec(rhs), this (op))
      }
      case UnaryExpr(rhs, op)                => {
        sink.UnaryExpr(spec(rhs), this (op))
      }
      case Cond(p, tt, ff)                   => {
        sink.Cond(spec(p), spec(tt), spec(ff))
      }
      case Quantified(quantifier, decls, pred, triggers) => {
        sink.Quantified(this(quantifier), this (decls), spec(pred), triggers.map(_.map(spec(_))))
      }
      case p: Location                       => spec(p)
      case x: PrimitiveFunction              => spec(x)
      case Invoke(_, _, _, _)                => {
        Errors.fail("AnchorToSink", "Calls cannot appear in spec")
      }
      case CAS(_, _, _, _)                   => {
        Errors.fail("AnchorToSink", "CAS cannot appear in spec")
      }
      case Old(l)                  => sink.Old(spec(l))
      case x@BuiltInFunctionCall(name, types, args) => sink.BuiltInFunctionCall(name, x.inferredTypes.map(this(_)), args.map(spec(_)))

    }, x.pos)
  }

  private def apply(x : Quantifier): sink.Quantifier = {
    x match {
      case ForAll() => sink.ForAll()
      case Exists() => sink.Exists()
    }
  }

  private def apply(decls: List[VarDecl]): List[sink.VarDecl] = {
    decls.map(d => pos(sink.VarDecl(this (d.t), d.name), d))
  }

  private def spec(x: PrimitiveFunction): sink.Expr = {
    pos(x match {
      case x@Length(a)             => {
        a.t match {
          case ArrayType(_,_,_)               => sink.Length(spec(a))
          case x@CollectionType("Seq", args)   => sink.BuiltInFunctionCall("SeqLen", args.map(this(_)), List(spec(a)))
          case _ => assert(false);         sink.Length(spec(a))
        }

      }
      case x@Lock(a)             => sink.Lock(spec(a))
      case x@Holds(e, t)           => sink.Holds(spec(e), spec(t))
      case x@IsLocal(e, t)         => sink.IsLocal(spec(e), spec(t))
      case x@IsShared(e)           => sink.IsShared(spec(e))
      case x@IsFresh(e)           => sink.IsFresh(spec(e))
      case x@NextCASSucceeds(e, t) => Errors.fail("AnchorToSink", "NextCASSucceeds not used anymore")
      case Rand()                  => Errors.fail("AnchorToSink", "Rand cannot appear in spec")
      case Alloc(_, _, _)             => Errors.fail("AnchorToSink", "Alloc cannot appear in spec")
      case AAlloc(_, _)            => Errors.fail("AnchorToSink", "AAlloc cannot appear in spec")
      case NextSpecStep(n) => sink.NextSpecStep(n)
    }, x.pos)
  }


  def apply(x: Type): sink.Type = {
    pos(x match {
      case IntType()                            => sink.IntType()
      case TidType()                            => sink.TidType()
      case VoidType()                           => sink.VoidType()
      case BoolType()                           => sink.BoolType()
      case BoogieType(name)                     => sink.BoogieType(name)
      case MoverType()                          => sink.MoverType()
      case ClassType(ident)                     => sink.ClassType(ident)
      case ArrayType(enclosing, ident, thisVar) => sink.ArrayType(enclosing, ident, pos(this (thisVar), thisVar.pos))
      case TypeVar(x) => sink.TypeVar(x)
      case CollectionType(name, typeArgs) => sink.CollectionType(name, typeArgs.map(this(_)))

    }, x.pos)
  }

//  def apply(x : CollectionKind) : sink.CollectionKind = {
//    x match {
//      case CollectionSet()      => sink.CollectionSet()
//      case CollectionSeq()      => sink.CollectionSeq()
//      case CollectionMultiset() => sink.CollectionMultiset()
//      case CollectionMap()      => sink.CollectionMap()
//    }
//  }

}

object AnchorToSink {
  def apply(x: Program): sink.Program = {
    val p = new AnchorToSink()(x)
//    println(sink.PrettyPrint.pp(p))
    anchor.sink.FrontEnd.tc(p)
  }
}
