package anchor.lang


class PrettyPrint {
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
    s"${ts} ${x.pred}"
  }

  def pp(x: ClassDecl): String =
  { s"class ${pp(x.name)} {${push()}${newLine()}${(x.arrays.map(pp(_)) ++ x.fields.map(pp(_)) ++ x.invariants.map(x => s"invariant ${pp(x)};") ++ x.methods.map(pp(_))).mkString(newLine())}${pop()}${newLine()}}" }

  def pp(x: FieldDecl): String = { s"${if (x.isVolatile) "volatile " else ""}${pp(x.t)} ${pp(x.name)} ${pp(x.spec)};${newLine()}" }


  def pp(x: MoverSpec, path: Int) : String = {
    def pph(x: Expr, path: Int) : List[String] = {
      x match {
        case Cond(p, t, f) => {
          if (path % 2 == 1) {
            s"(${PrettyPrint.pp(p)})" :: pph(t, path / 2)
          } else {
            s"(!${PrettyPrint.pp(p)})" :: pph(f, path / 2)
          }
        }
        case UnaryExpr(expr, Paren()) => {
          List(s"(${pph(expr, path)})")
        }
        case x => assert(path == 0); List(PrettyPrint.pp(x))
      }
    }
    val result = pph(x.conditionalMover,path)
    result.dropRight(1).mkString(" && ") + "  ==>  " + result.last
  }


  def pp(x : MoverSpec): String = {
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
    s"""${pp(x)}${if (x.isPublic) "public " else ""}${if (x.name == "init") "" else pp(x.returnType)} ${pp(x.name)}(${x.params.map(pp(_)).mkString(",")}) ${pp(x.stmt)}${newLine()}"""
  }

  def pp(x: ArrayDecl): String = {
    s"array ${x.name} = ${pp(x.elemType)}[${pp(x.spec)}]${newLine()}"
  }

  def pp(x: VarDecl): String = { s"${pp(x.t)} ${pp(x.name)}" }

  def pp(x: Stmt): String = {
    x match {
      case VarDeclStmt(v, None)          => s"${pp(v)};"
      case VarDeclStmt(v, Some(e))       => s"${pp(v)} = ${pp(e)};"
      case Assign(lhs, rhs)              => s"${pp(lhs)} = ${pp(rhs)};"
      case LocalAssign(assigns)          => assigns.map(pp(_)).mkString("", ", ", ";")
      case Block(None, body)             => s"{${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case Block(Some(n), body)          => s"${n}: {${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case ExprStmt(i)                   => s"${pp(i)};"
      case Return(None, false)           => "return;"
      case Return(Some(e), false)        => s"return ${pp(e)};"
      case Return(None, true)            => s"// return;"
      case Return(Some(e), true)         => s"// return ${pp(e)};"
      case SyncBlock(lock, stmt, _)      => s"synchronized (${pp(lock)}) ${pp(stmt)}"
      case If(cond, t, f)                => s"if (${pp(cond)}) ${pp(t)} else ${pp(f)}"
      case While(cond, stmt, invs, decs) => s"while (${pp(cond)}) ${specExprList("invariant", invs)} ${specExprList("decreases", decs)} ${pp(stmt)}"
      case Break(None)                   => "break;"
      case Break(Some(label))            => s"break ${label};"
      case Yield(Nil)                    => "yield;"
      case Yield(ensures)                => s"yield ${specExprList("ensures", ensures)}"
      case Commit()                      => "commit;"
      case Assume(expr)                  => s"assume ${pp(expr)};"
      case Assert(expr)                  => s"assert ${pp(expr)};"
      case Invariant(expr)               => s"invariant ${pp(expr)};"
      case BoogieCode(s)                 => "## " + s
      case NoReductionCheck(stmt)        => s"nocheck ${pp(stmt)}"
      case SyncStmt(op, x)                    => s"${pp(op)}(${pp(x)});"
    }
  }

  def pp(x : SyncOp): String = {
    x match {
      case Acquire() => "acquire"
      case Release() => "release"
      case Wait()    => "wait"
      case Notify()  => "notify"
    }
  }

  def specExprList(prefix: String, es: List[Expr]): String = {
    if (es == Nil) {
      ""
    } else {
      s"""${push}${es.map(i => s"${newLine()}${prefix} ${pp(i)};").mkString}${pop()}${newLine()}"""
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
      case NullConst()  => s"null"
      case NullTid()     => s"Tid.null"
      case MoverConst(m) => pp(m)
      case EmptyCollectionConst(t) => t.defaultValue()
    }, 20, Non())
  }

  def ppWithParens(x: PrimitiveFunction): Result = {
    x match {
      case Length(a)               => Result(s"${pp(a)}.length", 20, Non())
      case Lock(a)                 => Result(s"${pp(a)}.lock", 20, Non())
      case Holds(e, tid)           => Result(s"holds(${pp(e)}, ${pp(tid)})", 20, Non())
      case IsLocal(e, tid)         => Result(s"isLocal(${pp(e)}, ${pp(tid)})", 20, Non())
      case IsShared(e)             => Result(s"isShared(${pp(e)})", 20, Non())
      case IsFresh(e)             => Result(s"isFresh(${pp(e)})", 20, Non())
      case NextCASSucceeds(l, t)   => Result(s"casOK(${pp(l)}, ${pp(t)})", 20, Non())
      case Rand()                  => Result("*", 20, Non())
      case NextSpecStep(n)         => Result(s"nextSpecState(${n})", 20, Non())
      case Alloc(name, args, invs) => Result(s"new ${pp(name)}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)}", 13, Right())
      case AAlloc(a, size)         => Result(s"new ${pp(a)}(${pp(size)})", 13, Right())
    }
  }

  def ppWithParens(x : Location) : Result = {
    Result(x match {
      case VarAccess(name)            => name
      case FieldAccess(v, name, isStable)       => s"${pp(v)}${if (isStable) "." else "#"}${pp(name)}"
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
    x match {
      case ConstExpr(const)                => ppWithParens(const)
      case BinaryExpr(lhs, rhs, op)        => {
        val l = ppWithParens(lhs)
        val r = ppWithParens(rhs)
        val (prec, assoc) = desc(op)
        val parenLeft = l.prec < prec || l.prec == prec && assoc == Right()
        val parenRight = r.prec < prec || r.prec == prec && assoc == Left()
        Result(s"""${if (parenLeft) s"(${l.p})" else l.p} ${pp(op)} ${if (parenRight) s"(${r.p})" else r.p}""", prec, assoc)
      }
      case UnaryExpr(expr, op)               => {
        val e = ppWithParens(expr)
        val (prec, assoc) = desc(op)
        Result(if (prec > e.prec) {
          s"(${e.p})"
        } else {
          e.p
        }, prec, assoc)
      }
      case Invoke(ref, method, args, invs)   => {
        Result(s"${pp(ref)}.${method}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)}", 20, Non())
      }
      case location: Location                => ppWithParens(location)
      case CAS(lhs, field, expected, rhs)    => {
        Result(s"CAS(${pp(lhs)}, ${pp(field)}, ${pp(expected)}, ${pp(rhs)})", 20, Right())
      }
      case Quantified(quantifier, decls, pred, triggers) => {
        val ts = triggers.map(_.mkString(" { ", ", ", " } ")).mkString("")
        val q = quantifier match {
          case ForAll() => "forall"
          case Exists() => "exists"
        }
        Result(s"${q} ${decls.map(pp(_)).mkString(",")} ::${ts}{pp(pred)}", 1, Right())
      }
      case Cond(p, tt, ff)                   => {
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

      case x: PrimitiveFunction => ppWithParens(x)
      case BuiltInFunctionCall(ident, None, args) => Result(s"${ident}(${args.mkString(",")})", 20, Non())
      case BuiltInFunctionCall(ident, Some(typeArgs), args) => Result(s"${ident}<${typeArgs.map(pp(_)).mkString(",")}>(${args.map(pp(_)).mkString(",")})", 20, Non())
      case Old(l)                     => Result(s"old(${pp(l)})", 20, Non())
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
      case Neg() => "-"
      case Not() => "!"
      case _     => ""
    }
  }
//
//
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
      case CollectionType(name, Nil) => name
      case CollectionType(name, args) => s"${name}<${args.map(pp(_)).mkString(",")}>"
      case TypeVar(n)                            => n
    }
  }
}

object PrettyPrint {
  def pp(x: Program): String = {
    new PrettyPrint().pp(x)
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

  def pp(x: MoverSpec): String = {
    (new PrettyPrint()).pp(x)
  }
  def pp(x: MoverSpec, path: Int): String = {
    (new PrettyPrint()).pp(x, path)
  }
}
