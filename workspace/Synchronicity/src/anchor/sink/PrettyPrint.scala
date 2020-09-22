

package anchor.sink

//import anchor.transforms.CanonicalProgram

class PrettyPrint(val printPositions : Boolean = false) {
  var indentLevel = 0;

  def push(): String = {
    indentLevel += 1
    ""
  }

  def pop(): String = {
    indentLevel -= 1
    ""
  }

  def newLine(): String = {
    return "\n" + ("  " * indentLevel)
  }

  def pp(x: String): String = {
    x
  }

  def pp(x: Program): String = { x.axioms.map(c => pp(c) + ";").mkString("\n", "\n", "\n") + x.classes.map(c => pp(c)).mkString("\n") + x.globals.map(c => pp(c) + ";").mkString("\n", "\n", "\n") }


  def pp(x: ClassInvariant) : String = {
    val ts = x.triggers.map(_.mkString(" { ", ", ", " } ")).mkString("")
    s"${ts} ${pp(x.pred)}"
  }

  def pp(x: ClassDecl): String =
    { s"class ${pp(x.name)} {${push()}${newLine()}${(x.arrays.map(pp(_)) ++ x.fields.map(pp(_)) ++ x.invariants.map(x => s"invariant ${pp(x)};") ++ x.methods.map(pp(_))).mkString(newLine())}${pop()}${newLine()}}" }

  def pp(x: FieldModifier): String = {
    x match {
      case VolatileModifier() => "volatile"
      case ABAFreeModifier()  => "noABA"
      case InternalModifier() => s"updatesViaCAS"
      case HasCASOperationModifier() => "hasCASOperation"
    }
  }

  def pp(x: FieldDecl): String = { s"${x.modifiers.map(pp(_)).mkString("", " ", " ")}${pp(x.t)} ${pp(x.name)} ${pp(x.spec)}${newLine()}" }


  def pp(x: Spec, path: Int) : String = {
    def pph(x: Expr, path: Int) : List[String] = {
      x match {
        case Cond(p, t, f) => {
          if (path % 2 == 1) {
            s"(${PrettyPrint.pp(p)})" :: pph(t, path / 2)
          } else {
            s"${PrettyPrint.pp(AST.not(p))}" :: pph(f, path / 2)
          }
        }
//        case UnaryExpr(expr, Paren()) => {
//          pph(expr, path)
//        }
        case x => assert(path == 0); List(PrettyPrint.pp(x))
      }
    }
    val result = pph(x.conditionalMover,path)
    result.dropRight(1).mkString(" && ") + "  ==>  " + result.last
  }


  def pp(x : Spec): String = {
    pp(x.conditionalMover) + (if (x.blocking) " !" else "") +
      specExprList("yields_as", x.yieldsAs)
  }

  def pp(x : Mover): String = {
    x match {
      case I() => "I"
      case B() => "B"
      case L() => "L"
      case R() => "R"
      case N() => "N"
      case E() => "E"
    }
  }

  private def pp(x: VarAccess) = {
    x.name
  }

  private def pp(x: Transaction): String = {
    val items =
      x.modifies.map(r => s"modifies ${pp(r)};") ++ x.ensures.map(r => s"ensures ${pp(r)};")
    s"""{${push();newLine()}${items.mkString(s"${newLine()}")}${pop();newLine()}}${if(x.repeats) "*" else ""}"""
  }

  def pp(x: ExplicitMethodSpec): String = {
    val requires = x.requires.map(r => s"requires ${pp(r)};").mkString(s"${newLine()}", s"${newLine()}", s"${newLine()}")

    val ensures = x.transactions.map(e => pp(e))
    requires + ensures.mkString("", s"${push();newLine()}yield;${pop();newLine()}", s"${newLine()}")
  }

  def pp(x: MethodDecl): String = {
    s"""${pp(x.spec)}${if (x.isPublic) "public " else ""}${pp(x.returnType)} ${pp(x.name)}(${x.params.map(pp(_)).mkString(",")}) ${pp(x.stmt)}${newLine()}"""
  }

  def pp(x: ArrayDecl): String = {
    s"array ${x.name} = ${pp(x.elemType)}[${pp(x.spec)}]${newLine()}"
  }

  def pp(x: VarDecl): String = { s"${pp(x.t)} ${pp(x.name)}" }

  def pp(x: Stmt): String = {
    val result = x match {
      case VarDeclStmt(v)                                   => s"${pp(v)};"
      case Assign(lhs, rhs)                                 => s"${lhs.map(pp(_)).mkString(", ")} = ${rhs.map(pp(_)).mkString(", ")};"
      case Block(None, body)                       => s"{${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case Block(Some(n), body)                    => s"${n}: {${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case Write(lhs, field, rhs, None)                           => s"${pp(lhs)}.${pp(field)} := ${pp(rhs)};"
      case Write(lhs, field, rhs, Some(movesAs))                           => s"${pp(lhs)}.${pp(field)} := ${pp(rhs)} as ${pp(movesAs)};"
      case LocalWrites(writes) => writes.map(pp(_)).map(_.dropRight(1)).mkString("", ", ", ";")
      case Read(lhs, rhs, field, None)                            => s"${pp(lhs)} := ${pp(rhs)}.${pp(field)};"
      case Read(lhs, rhs, field,  Some(movesAs))                            => s"${pp(lhs)} := ${pp(rhs)}.${pp(field)}  as ${pp(movesAs)};"
      case Invoke(ref, method, args, Some(v), invs)         => s"${pp(v)} = ${pp(ref)}.${pp(method)}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)}"
      case Invoke(ref, method, args, None, invs)            => s"${pp(ref)}.${pp(method)}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)}"
      case InlineInvoke(invoke) => s"inlined ${pp(invoke)};"
      case InlineReturn() => s"inlined return;"
      case Return(None, b)                              => s"${if (b) "//" else ""} return;"
      case Return(Some(e), b)                           => s"${if (b) "//" else ""} return ${pp(e)};"
      case Sync(lock, stmt, _)                    => s"synchronized (${pp(lock)}) ${pp(stmt)}"
      case If(cond, t, f)                         => s"if (${pp(cond)}) ${pp(t)} else ${pp(f)}"
      case While(cond, stmt, invs, decreases)                => s"while (${pp(cond)}) ${specExprList("invariant", invs)} ${specExprList("decreases", decreases)} ${pp(stmt)}"
      case Break(None)                            => "break;"
      case Break(Some(label))                     => s"break ${label};"
      case CAS(result, lhs, field, expected, rhs) => s"${pp(result)} = ${pp(lhs)}.${pp(field)}@${pp(expected)} :~ ${pp(rhs)};"
      case Alloc(lhs, name)                       => s"${pp(lhs)} = new ${pp(name)}();"
      case Yield(Nil)                             => "yield;"
      case Yield(ensures)                         => s"yield ${specExprList("ensures", ensures)}"
      case Commit() => "commit;"
      case Assume(expr)                           => s"assume ${pp(expr)};"
      case Assert(expr)                           => s"assert ${pp(expr)};"
      case Invariant(expr)                        => s"invariant ${pp(expr)};"
      case AWrite(lhs, i, rhs)                    => s"${pp(lhs)}[${pp(i)}] := ${pp(rhs)};"
      case ARead(lhs, rhs, i)   => s"${pp(lhs)} := ${pp(rhs)}[${pp(i)}];"
      case AAlloc(lhs, t, size) => s"${pp(lhs)} = new ${pp(t)}(${pp(size)});"
      case BoogieCode(s)        => "## " + s
      case NoReductionCheck(s)  => s"nocheck ${pp(s)}"
      case Acquire(x)           => s"acquire(${pp(x)});"
      case Release(x)           => s"release(${pp(x)});"
    }
    if (printPositions) {
      s"(${x.pos}: ${result})"
    } else {
      result
    }
  }

  def specExprList(prefix: String, es: List[Expr]): String = {
    if (es == Nil) {
      ""
    } else {
      s"""${push}${es.map(i => s"${newLine()}${prefix} ${pp(i)}").mkString("", ";", ";")}${pop()}${newLine()}"""
    }
  }

  sealed abstract class Assoc()
  case class Left() extends Assoc()
  case class Right() extends Assoc()
  case class Non() extends Assoc()

  def desc(op: BinaryOp): (Int, Assoc) = {
    op match {
      case Add()     => (14, Left())
      case Sub()     => (14, Left())
      case Mul()     => (12, Left())
      case Div()     => (12, Left())
      case Mod()     => (12, Left())
      case And()     => (4, Left())
      case Or()      => (3, Left())
      case EQ()      => (8, Left())
      case NE()      => (8, Left())
      case LT()      => (9, Non())
      case GT()      => (9, Non())
      case LE()      => (9, Non())
      case GE()      => (9, Non())
      case Implies() => (1, Right())
    }
  }

  def desc(x: UnaryOp): (Int, Assoc) = {
    x match {
      case Neg() => (14, Right())
      case Not() => (14, Right())
      case Paren() => (17, Non())
    }
  }

  case class Result(p: String, prec: Int, assoc: Assoc)

  def ppWithParens(x: Const): Result = {
    Result(x match {
      case IntConst(v)   => v.toString()
      case BoolConst(v)  => v.toString()
      case NullConst(t)  => s"${pp(t)}.null"
      case NullTid()     => s"Tid.null"
      case MoverConst(m) => pp(m)
      case EmptyCollectionConst(t) =>  s"${t.name}Empty"
    }, 20, Non())
  }

  def ppWithParens(x: PrimitiveFunction): Result = {
    x match {
      case Length(a)             => Result(s"${pp(a)}.length", 20, Non())
      case Lock(a)             => Result(s"${pp(a)}.lock", 20, Non())
      case Holds(e, tid)         => Result(s"holds(${pp(e)}, ${pp(tid)})", 20, Non())
      case IsLocal(e, tid)       => Result(s"isLocal(${pp(e)}, ${pp(tid)})", 20, Non())
      case IsShared(e)           => Result(s"isShared(${pp(e)})", 20, Non())
      case IsFresh(e)           => Result(s"isFresh(${pp(e)})", 20, Non())
      case MoverPermission(l, None) => Result(s"readPermission(${pp(l)})", 20, Non())
      case MoverPermission(l, Some(v)) => Result(s"writePermission(${pp(l)}, ${pp(v)})", 20, Non())
      case GoesWrong(e) =>      Result(s"goesWrong(${pp(e)})", 20, Non())
      case Rand()                => Result("*", 20, Non())
      case NextSpecStep(n) => Result(s"NextSpecStep(${n})", 20, Non())
    }
  }

  def ppWithParens(x : Location) : Result = {
    Result(x match {
      case VarAccess(name)            => name
      case FieldAccess(v, name)       => s"${pp(v)}.${pp(name)}"
      case ArrayAccess(l, index)      => s"${pp(l)}[${pp(index)}]"
    }, 20, Non())
  }

  def doP(b: Boolean, s: String) = {
    if (b) {
      s"(${s})"
    } else {
      s
    }
  }

  def ppWithParens(x: Expr): Result = {
    val result = x match {
      case ConstExpr(const)              => ppWithParens(const)
      case BinaryExpr(lhs, rhs, op)      => {
        val l = ppWithParens(lhs)
        val r = ppWithParens(rhs)
        val (prec, assoc) = desc(op)
        val parenLeft = l.prec < prec || l.prec == prec && assoc == Right()
        val parenRight = r.prec < prec || r.prec == prec && assoc == Left()
        Result(s"""${if (parenLeft) s"(${l.p})" else l.p} ${pp(op)} ${if (parenRight) s"(${r.p})" else r.p}""", prec, assoc)
      }
      case UnaryExpr(expr, op)           => {
        val e = ppWithParens(expr)
        val (prec, assoc) = desc(op)
        Result(if (true || prec > e.prec) {
          s"${pp(op)}(${e.p})"
        } else {
          s"${pp(op)}${e.p}"
        }, prec, assoc)
      }
      case location: Location            => ppWithParens(location)
      case Quantified(quantifier, decls, pred, triggers) => {
        val ts = triggers.map(_.mkString(" { ", ", ", " } ")).mkString("")
        val q = quantifier match {
          case ForAll() => "forall"
          case Exists() => "exists"
        }
        Result(s"${q} ${decls.map(pp(_)).mkString(",")} ::${ts}${pp(pred)}", 1, Right())
      }
      case Cond(p, tt, ff)               => {
        val pp = ppWithParens(p)
        val isNested = (tt match {
          case Cond(_,_,_) | UnaryExpr(Cond(_,_,_), Paren())            => true
          case _ => false
        }) || (ff match {
          case Cond(_, _, _) | UnaryExpr(Cond(_, _, _), Paren()) => true
          case _                                                 => false
        })
        if (isNested) push()
        val ttt = ppWithParens(tt)
        val fff = ppWithParens(ff)
        val parenPP = pp.prec < 2 || pp.prec == 2
        val parenTT = ttt.prec < 2
        val parenFF = fff.prec < 2
        if (isNested) pop()
        def nested(s: => String) = {
          if (isNested) s else ""
        }

        Result(s"""${doP(parenPP, pp.p)}${nested(s"${newLine()}")} ? ${doP(parenTT, ttt.p)}${nested(newLine())} : ${doP(parenFF, fff.p)}""", 2, Right())
      }

      case x: PrimitiveFunction     => ppWithParens(x)
      case LoweredExpr(e, original) => {
        val p = ppWithParens(e)
        Result(s"${p.p} /* == ${pp(original)} */", p.prec, p.assoc)
      }
      case Old(l)                     => Result(s"old(${pp(l)})", 20, Non())
      case BuiltInFunctionCall(ident, typeArgs, args) => Result(s"${ident}<${typeArgs.map(pp(_)).mkString(",")}>(${args.map(pp(_)).mkString(",")})", 20, Non())

    }
    if (printPositions) {
      Result(s"(${x.pos}:(${result.p}))", result.prec, result.assoc)
    } else {
      result
    }
  }

  def pp(x: Expr): String = {
    ppWithParens(x).p
  }


  def pp(x: BinaryOp): String = {
    x match {
      case Add()     => "+"
      case Sub()     => "-"
      case Mul()     => "*"
      case Div()     => "/"
      case Mod()     => "%"
      case EQ()      => "=="
      case LT()      => "<"
      case GT()      => ">"
      case LE()      => "<="
      case GE()      => ">="
      case NE()      => "!="
      case And()     => "&&"
      case Or()      => "||"
      case Implies() => "==>"
    }

  }
  def pp(x: UnaryOp): String = {
    x match {
      case Neg()   => "-"
      case Not()   => "!"
      case Paren() => ""
    }
  }
  def pp(x: Type): String = {
    x match {
      case IntType()                             => "int"
      case BoolType()                            => "bool"
      case VoidType()                            => "void"
      case ClassType(x)                          => pp(x)
      case TidType()                             => "Tid"
      case MoverType()                           => "Mover"
      case BoogieType(name)                      => name
      case ArrayType(enclosing, ident, thisName) => s"[${enclosing}.${ident}{${pp(thisName)}}]"
      case CollectionType(name, args) => s"${name}<${args.map(pp(_)).mkString(",")}>"
      case TypeVar(n)                            => n

    }
  }
}

object PrettyPrint {
  def pp(x: Program, pos : Boolean = false): String = {
    new PrettyPrint(pos).pp(x)
  }

  def pp(x: Expr): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: Stmt): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: Type): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: Mover): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: Spec): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: ExplicitMethodSpec): String = {
    (new PrettyPrint()).pp(x)
  }

  def pp(x: Spec, path: Int): String = {
    (new PrettyPrint()).pp(x, path)
  }
}
