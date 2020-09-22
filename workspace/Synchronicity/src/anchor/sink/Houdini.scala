package anchor.sink

import AST.pos
import acme.scala.Util
import anchor.sink.PrettyPrint.pp
import anchor.util.Errors
import anchor.transforms._

import scala.util.parsing.input._


class Houdini {

  var houndiniVars : List[VarDecl] = Nil

  def gen() = {
    val decl = VarDecl(BoolType(), Util.fresh("_b"))
    houndiniVars = decl :: houndiniVars
    decl
  }

  case class HoudiniContext(val vars : List[VarDecl],
                            val assignments : Set[Assign],
                            val exprs : Set[Expr]) {
    def add(v : VarDecl) = HoudiniContext(v::vars, assignments, exprs)
    def add(a : Assign) = HoudiniContext(vars, assignments + a, exprs)
    def add(e : Expr) = HoudiniContext(vars, assignments, exprs + e)
    def add(es : List[Expr]) = HoudiniContext(vars, assignments, exprs.union(es.toSet))

    def wrap(e : Expr): BinaryExpr = {
      val decl = gen()
      BinaryExpr(VarAccess(decl.name), e, Implies())
    }

    def wrap(es : List[Expr]) : List[Expr] = {
      es.map(e => wrap(e))
    }

    def dropReflexiveAndSymmetric(es: List[Expr]) = {
      es.foldRight (List[Expr]()) { case (e,r) => e match {
        case e if r.contains(e) => r
        case BinaryExpr(lhs, rhs, op) if lhs == rhs => r
        case BinaryExpr(lhs, rhs, EQ()) if r.contains(BinaryExpr(rhs, lhs, EQ())) => r
        case BinaryExpr(lhs, rhs, GT()) if r.contains(BinaryExpr(rhs, lhs, LE())) => r
        case BinaryExpr(lhs, rhs, GE()) if r.contains(BinaryExpr(rhs, lhs, LT())) => r
        case BinaryExpr(lhs, rhs, LT()) if r.contains(BinaryExpr(rhs, lhs, GE())) => r
        case BinaryExpr(lhs, rhs, LE()) if r.contains(BinaryExpr(rhs, lhs, GT())) => r
        case e => e::r
      }}
    }

    def conjectures(p : Position) : List[Expr] = {
      val holds : List[Expr] = vars.filter(_.t.isInstanceOf[ClassType]).map(v => Parser.expr(s"${v.name}._lock == tid"))
      val isLocal : List[Expr] = vars.filter(_.t.isInstanceOf[ClassType]).map(v => IsLocal(VarAccess(v.name), VarAccess("tid")))
      val isShared : List[Expr] = vars.filter(_.t.isInstanceOf[ClassType]).map(v => IsShared(VarAccess(v.name)))

      val varsByType = vars.filter(_.t.isInstanceOf[ClassType]).groupBy(_.t.asInstanceOf[ClassType].decl)

      (wrap(dropReflexiveAndSymmetric(
        holds
            ++
        isLocal
            ++ isShared
            ++ expressions()
         //   ++ equalities()
      )))
    }

    def equalities() = {
      val varsByType = vars.filter(_.t.isInstanceOf[ClassType]).groupBy(_.t.asInstanceOf[ClassType].decl)
      val vs = for ((t, vars) <- varsByType;
           a <- vars;
           b <- vars if a != b) yield
        BinaryExpr(new VarAccess(a), new VarAccess(b), EQ())


      val fields = for (a <- vars if a.t.isInstanceOf[ClassType];
                        f <- a.t.asInstanceOf[ClassType].decl.fields if (!f.name.startsWith("_"));
                        b <- vars if b.t == f.t) yield
        BinaryExpr(FieldAccess(new VarAccess(a), f.name), new VarAccess(b), EQ())


      (vs ++ fields)

    }

    def varsWithType(t: Type) : List[VarAccess] = {
      vars.filter(t == _.t).map(p => VarAccess(p.name))
    }

    def fieldsWithType(x : VarDecl, t : Type): List[Expr] = {
      x.t match {
        case c: ClassType => {
          val sets : List[List[Expr]] = for (f <- c.decl.fields if (!f.name.startsWith("_"))
                                                   if f.t == t) yield List[Expr](FieldAccess(new VarAccess(x), f.name))
          sets.flatten
        }
        case a: ArrayType => {
          List[Expr](Length(new VarAccess(x)))
        }
        case _ => Nil
      }
    }

    def exprsWithType(t: Type) : List[Expr] = {
      val vars = varsWithType(t)
      val fields = this.vars.flatMap(v => fieldsWithType(v, t))
      val r = vars ++ fields
      r
    }

    def expr(e : Expr) : List[Expr] = {
      assert (e.t == BoolType())
      e match {
        case BinaryExpr(lhs, rhs, op) if lhs.t == IntType() => {
          val all = for (l <- exprsWithType(lhs.t);
                         r <- exprsWithType(rhs.t) if l != r) yield {
            List(EQ(), NE(), LT(), LE(), GT(), GE()).map(AST.binary(l,r,_))
          }
          all.flatten
        }
        case BinaryExpr(lhs, rhs, op) => {
          val all = for (l <- exprsWithType(lhs.t);
                         r <- exprsWithType(rhs.t) if l != r) yield {
            List(EQ(), NE()).map(AST.binary(l,r,_))
          }
          all.flatten
        }
        case UnaryExpr(expr, op)         => {
          val all = for (l <- varsWithType(expr.t)) yield {
            List[Expr](AST.unary(l, op), AST.not(AST.unary(l, op)))
          }
          all.flatten
        }
        case _ => Nil
      }
    }

    def expressions() : Set[Expr] = {
      this.exprs.map(expr(_)).flatten
    }
  }


  def apply(x: Program): Program = {
    pos(Program(x.classes.map(c => this(c)), (x.globals ++ houndiniVars).map(c => this(c)), x.axioms.map(this(_)), new Library(x.library.collections.mapValues(this(_)))), x.pos)
  }

  def apply(x: Collection) : Collection = {
    new Collection(x.name, x.numParams, x.functions.map(this(_)), x.boogieText)
  }


  def apply(x: BuiltInFunctionDecl) : BuiltInFunctionDecl = {
    pos(BuiltInFunctionDecl(x.name, x.typeVars, x.parameters.map(this(_)), this(x.returnType)), x.pos)
  }

  def apply(x: ClassDecl): ClassDecl = {
    pos(ClassDecl(x.name, x.arrays.map(this(_)), x.fields.map(this(_)), x.methods.map(this(_)), x.invariants.map(this(_))), x.pos)
  }

  def apply(x : ClassInvariant) : ClassInvariant = {
    pos(ClassInvariant(this(x.pred), x.triggers.map(_.map(this(_)))), x.pos)
  }


  def apply(x: ArrayDecl): ArrayDecl = {
    pos(ArrayDecl(x.name, this(x.elemType), x.elemName, this(x.spec)), x.pos)
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
    pos(FieldDecl(this(x.t), x.name, this(x.spec), x.modifiers.map(this(_))), x.pos)
  }


  def apply(x : Spec): Spec = {
    pos(Spec(this(x.conditionalMover), x.blocking, x.yieldsAs.map(this(_))), x.pos)
  }

  def apply(x: VarAccess): VarAccess = { pos(VarAccess(x.name), x.pos) }

  def apply(x: Location): Location = {
    pos(x match {
      case x@VarAccess(l)          => this(x)
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
    val stmt = if (x.isPublic) {
      val empty = HoudiniContext(x.stmt.scope.allLocals, Set.empty, Set.empty)
      this (empty, x.stmt)._2
    } else {
      DeepCopyWithPositions(x.stmt)
    }
    pos(MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), this(x.spec), stmt),
      x.pos)
  }

  def pos2(x : (HoudiniContext, Stmt), p: Position) = {
    (x._1, AST.pos(x._2, p))
  }

  def conjectures(ctxt : HoudiniContext, x : Stmt) : List[Expr] = {
    ctxt.conjectures(x.pos)
  }


  def apply(ctxt: HoudiniContext, x: Stmt): (HoudiniContext, Stmt) = {
    pos2(x match {
      case VarDeclStmt(v)                                   => (if (v.name.startsWith("tmp")) ctxt else ctxt.add(v), VarDeclStmt(this (v)))
      case x@Assign(lhs, rhs)                               => (ctxt.add(x), Assign(lhs.map(this (_)), rhs.map(this (_))))
      case Block(n, body)                          => {
         //var ctxt2 = if (!inlined.isDefined) ctxt else HoudiniContext(Nil, ctxt.assignments, ctxt.exprs)
        var ctxt2 = ctxt
        var body2: List[Stmt] = Nil
        for (s <- body) {
          val (c2, s2) = this (ctxt2, s)
          ctxt2 = c2
          body2 = body2 :+ s2
        }
        (ctxt2, Block(n, body2))
      }
      case Write(lhs, field, rhs, movesAs)                           => (ctxt, Write(this (lhs), field, this (rhs), movesAs))
      case LocalWrites(writes) => {
        var ctxt2 = ctxt
        var ws : List[Write] = Nil
        for (w <- writes) {
          val (c, s) = this (ctxt2, w)
          ctxt2 = c
          ws = ws :+ w
        }
        (ctxt2, LocalWrites(ws))
      }
      case Read(lhs, rhs, field, movesAs)                            => (ctxt, Read(this (lhs), this (rhs), field, movesAs))
      case Invoke(ref, method, args, res, invs)             => (ctxt, Invoke(this (ref), method, this (args), res.map(this (_)), invs.map(this (_))))
      case InlineInvoke(invoke) =>
        val (ctxt2, invoke2) = this(ctxt, invoke)
        (ctxt2, InlineInvoke(invoke2.asInstanceOf[Invoke]))
      case InlineReturn() =>
        (ctxt, InlineReturn())

      case Return(e, isSynthetic)                           => (ctxt, Return(e.map(this (_)), isSynthetic))
      case Sync(lock, stmt, pos)                            => {
        val (ctxt2, stmt2) = this (ctxt, stmt)
        (ctxt2, Sync(this (lock), stmt2, pos))
      }
      case If(LoweredExpr(cond, orig), t, f)                => {
        val ctxt2 = ctxt.add(cond).add(orig)
        (ctxt2, If(this (cond), this (ctxt2, t)._2, this (ctxt2, f)._2))
      }
      case While(LoweredExpr(cond, orig), stmt, invs, decreases) => {
        val ctxt2 = ctxt.add(orig).add(cond).add(invs)
        val conjectures = this.conjectures(ctxt2, x).map(DeepCopyWithPositions(_))
        val (ctxt3, stmt3) = this (ctxt2, stmt)
        (ctxt3, While(this (cond), stmt3, this (invs) ++ conjectures, decreases.map(this(_))))
      }
      case If(cond, t, f)                             => {
        val ctxt2 = ctxt.add(cond)
        (ctxt2, If(this (cond), this (ctxt2, t)._2, this (ctxt2, f)._2))
      }
      case While(cond, stmt, invs, decreases)                    => {
        val ctxt2 = ctxt.add(cond).add(invs)
        val (ctxt3, stmt3) = this (ctxt2, stmt)
        (ctxt3, While(this (cond), stmt3, this (invs), decreases.map(this(_))))
      }
      case Break(l)                                   => (ctxt, Break(l))
      case CAS(result, lhs, field, expected, rhs)     => (ctxt, CAS(this (result), this (lhs), field, this (expected), this (rhs)))
      case Alloc(lhs, name)                           => (ctxt, Alloc(this (lhs), name))
      case Yield(ensures)                             => (ctxt.add(ensures), Yield(this (ensures)))
      case Commit() => (ctxt, Commit())
      case Assume(x)                                  => (ctxt.add(x), Assume(this (x)))
      case Assert(LoweredExpr(x, original))           => (ctxt.add(x).add(original), Assert(this (x)))
      case Assert(x)                                  => (ctxt.add(x), Assert(this (x)))
      case Invariant(x)         => (ctxt.add(x), Invariant(this (x)))
      case AWrite(lhs, i, rhs)  => (ctxt, AWrite(this (lhs), this (i), this (rhs)))
      case ARead(lhs, rhs, i)   => (ctxt, ARead(this (lhs), this (rhs), this (i)))
      case AAlloc(lhs, a, size) => (ctxt, AAlloc(this (lhs), this (a).asInstanceOf[ArrayType], this (size)))
      case BoogieCode(s)        => (ctxt, BoogieCode(s))
      case NoReductionCheck(s)  => {
        val (ctxt2, stmt2) = this (ctxt, s)
        (ctxt2, NoReductionCheck(stmt2))
      }
      case Acquire(x)           => (ctxt, Acquire(this (x)))
      case Release(x)           => (ctxt, Release(this (x)))
    }, x.pos)
  }

  def apply(x: VarDecl): VarDecl = {
    pos(VarDecl(this(x.t), x.name), x.pos)
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

  def apply(xs: List[Expr]) : List[Expr] = xs.map(this(_))

  def apply(x: VarOrConst) : VarOrConst = {
    x match {
      case y : ConstExpr => apply(y)
      case y : VarAccess => apply(y)
    }
  }

  def apply(x: ConstExpr) : ConstExpr = {
    pos(ConstExpr(this (x.const)), x.pos)
  }

  def apply(x: Expr): Expr = {
    pos(x match {
      case x: ConstExpr                  => this (x)
      case BinaryExpr(lhs, rhs, op)      => BinaryExpr(this (lhs), this (rhs), this (op))
      case UnaryExpr(expr, op)           => UnaryExpr(this (expr), this (op))
      case Quantified(q, decls, pred, triggers) => Quantified(q, decls.map(this (_)), this (pred), triggers.map(_.map(this (_))))
      case Cond(p, tt, ff)               => Cond(this (p), this (tt), this (ff))
      case p: Location                   => this (p)
      case x: PrimitiveFunction          => this (x)
      case LoweredExpr(e, original)      => LoweredExpr(this (e), this (original))
      case Old(l)                => Old(this (l))
      case BuiltInFunctionCall(name, types, args) => BuiltInFunctionCall(name, types.map(this(_)), args.map(this(_)))


    }, x.pos)
  }

  def apply(x : BinaryOp) = {
    pos(x match {
      case Add()     => Add()
      case Sub()     => Sub()
      case Mul()     => Mul()
      case Div()     => Div()
      case Mod()     => Mod()
      case And()     => And()
      case Or()      => Or()
      case EQ()      => EQ()
      case NE()      => NE()
      case LT()      => LT()
      case GT()      => GT()
      case LE()      => LE()
      case GE()      => GE()
      case Implies() => Implies()
    }, x.pos)
  }

  def apply(x : UnaryOp) = {
    pos(x match {
      case Not() => Not()
      case Neg() => Neg()
      case Paren() => Paren()
    }, x.pos)
  }

  def apply(x: PrimitiveFunction): Expr = {
    pos(x match {
      case Length(a)             => Length(this (a))
      case Lock(a)             => Lock(this (a))
      case Holds(e, t)           => Parser.expr(s"${pp(e)}._lock == ${pp(t)}")
      case IsLocal(e, t)         => IsLocal(this (e), this (t))
      case IsShared(e)           => IsShared(this (e))
      case IsFresh(e)           => IsFresh(this (e))
      case MoverPermission(e, v) => MoverPermission(this (e), v.map(apply))
      case GoesWrong(e) => GoesWrong(this (e))
      case Rand()                => Rand()
      case NextSpecStep(n) => NextSpecStep(n)
    }, x.pos)
  }


  def apply(x: Type): Type = {
    pos(x match {
      case IntType() => IntType()
      case TidType() => TidType()
      case VoidType() => VoidType()
      case BoolType() => BoolType()
      case MoverType() => MoverType()
      case BoogieType(name) => BoogieType(name)
      case ClassType(ident) => ClassType(ident)
      case ArrayType(enclosing, ident, thisVar) =>
        ArrayType(enclosing, ident, this(thisVar))
      case TypeVar(x) => TypeVar(x)
      case CollectionType(name, typeArgs) => CollectionType(name, typeArgs.map(this(_)))
    }, x.pos)
  }

}

object Houdini {
  def apply(p : Program): Program = {
    new Houdini()(p)
  }
}
