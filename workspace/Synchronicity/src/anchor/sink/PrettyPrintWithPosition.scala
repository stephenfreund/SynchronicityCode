//
//
//package anchor.sink
//
////import anchor.transforms.CanonicalProgram
//
//class PrettyPrintWithPosition {
//  var indentLevel = 0;
//
//  def push(): String = {
//    indentLevel += 1
//    ""
//  }
//
//  def pop(): String = {
//    indentLevel -= 1
//    ""
//  }
//
//  def newLine(): String = {
//    return "\n" + ("  " * indentLevel)
//  }
//
//  def pp(x: String): String = {
//    x
//  }
//
//  def pp(x: Program): String = { x.classes.map(c => pp(c)).mkString("\n") + x.globals.map(c => pp(c) + ";").mkString("\n", "\n", "\n") }
//
//  def pp(x: ClassDecl): String =
//    { s"class ${pp(x.name)} {${push()}${newLine()}${(x.arrays.map(pp(_)) ++ x.fields.map(pp(_)) ++ x.invariants.map(x => s"invariant ${pp(x)};") ++ x.methods.map(pp(_))).mkString(newLine())}${pop()}${newLine()}}" }
//
//  def pp(x: FieldDecl): String = { s"${pp(x.t)} ${pp(x.name)} ${pp(x.spec)};${newLine()}" }
//
//
//  def pp(x: Spec, path: Int) : String = {
//    def pph(x: Expr, path: Int) : List[String] = {
//      x match {
//        case Cond(p, t, f) => {
//          if (path % 2 == 1) {
//            s"${PrettyPrintWithPosition.pp(p)}" :: pph(t, path / 2)
//          } else {
//            s"${PrettyPrintWithPosition.pp(AST.not(p))}" :: pph(f, path / 2)
//          }
//        }
//        case x => assert(path == 0); List(PrettyPrintWithPosition.pp(x))
//      }
//    }
//    val result = pph(x.conditionalMover,path)
//    result.dropRight(1).mkString(" && ") + "  ==>  " + result.last
//  }
//
//
//  def pp(x : Spec): String = {
//    pp(x.conditionalMover) + (if (x.blocking) " !" else "") +
//      specExprList("yields_as", x.yields_ass)
//  }
//
//  def pp(x : Mover): String = {
//    x match {
//      case B() => "B"
//      case L() => "L"
//      case R() => "R"
//      case N() => "N"
//      case E() => "E"
//    }
//  }
//
//  private def pp(x: VarAccess) = {
//    x.name
//  }
//
//  def pp(x: MethodDecl): String = {
//    s"""${x.requires.map(r => s"requires ${pp(r)};").mkString(s"${newLine()}", s"${newLine()}", s"${newLine()}")}${if (x.isPublic) "public " else ""}${pp(x.returnType)} ${pp(x.name)}(${x.params.map(pp(_)).mkString(",")}) ${pp(x.stmt)}${newLine()}"""
//  }
//
//  def pp(x: ArrayDecl): String = {
//    s"array ${x.name} = ${pp(x.elemType)}[${pp(x.spec)}]${newLine()}"
//  }
//
//  def pp(x: VarDecl): String = { s"${pp(x.t)} ${pp(x.name)}" }
//
//  def pp(x: Stmt): String = {
//    s"(${x.pos}:" + (x match {
//      case VarDeclStmt(v)                                   => s"${pp(v)};"
//      case Assign(lhs, rhs)                                 => s"${lhs.map(pp(_)).mkString(", ")} = ${rhs.map(pp(_)).mkString(", ")};"
//      case Block(None, body, inlined)                       => s"{${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
//      case Block(Some(n), body, inlined)                    => s"${n}: {${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
//      case Write(lhs, field, rhs)                           => s"${pp(lhs)}.${pp(field)} := ${pp(rhs)};"
//      case LocalWrites(writes) => writes.map(pp(_)).map(_.dropRight(1)).mkString("", ", ", ";")
//      case Read(lhs, rhs, field)                            => s"${pp(lhs)} := ${pp(rhs)}.${pp(field)};"
//      case Invoke(ref, method, args, Some(v), invs)         => s"${pp(v)} = ${pp(ref)}.${pp(method)}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)};"
//      case Invoke(ref, method, args, None, invs)            => s"${pp(ref)}.${pp(method)}(${args.map(pp(_)).mkString(",")}) ${specExprList("invariant", invs)};"
//      case Return(None, false)                              => "return;"
//      case Return(Some(e), false)                           => s"return ${pp(e)};"
//      case Return(_, true)                        => ""
//      case Sync(lock, stmt, _)                    => s"synchronized (${pp(lock)}) ${pp(stmt)}"
//      case If(cond, t, f)                         => s"if (${pp(cond)}) ${pp(t)} else ${pp(f)}"
//      case While(cond, stmt, invs, decreases)                => s"while (${pp(cond)}) ${specExprList("invariant", invs)} ${specExprList("decreases", invs)} ${pp(stmt)}"
//      case Break(None)                            => "break;"
//      case Break(Some(label))                     => s"break ${label};"
//      case CAS(result, lhs, field, expected, rhs) => s"${pp(result)} = ${pp(lhs)}.${pp(field)}@${pp(expected)} :~ ${pp(rhs)};"
//      case Alloc(lhs, name)                       => s"${pp(lhs)} = new ${pp(name)}();"
//      case Yield(Nil)                             => "yield;"
//      case Yield(ensures)                         => s"yield ${specExprList("ensures", ensures)}"
//      case Commit() => "commit;"
//      case Assume(expr)                           => s"assume ${pp(expr)};"
//      case Assert(expr)                           => s"assert ${pp(expr)};"
//      case Invariant(expr)                        => s"invariant ${pp(expr)};"
//      case AWrite(lhs, i, rhs)  => s"${pp(lhs)}[${pp(i)}] := ${pp(rhs)};"
//      case ARead(lhs, rhs, i)   => s"${pp(lhs)} := ${pp(rhs)}[${pp(i)}];"
//      case AAlloc(lhs, t, size) => s"${pp(lhs)} = new ${pp(t)}(${pp(size)});"
//      case BoogieCode(s)        => "## " + s
//      case NoReductionCheck(s)  => s"nocheck ${pp(s)}"
//      case Acquire(x)           => s"acquire(${pp(x)});"
//      case Release(x)           => s"release(${pp(x)});"
//    }) + ")"
//  }
//
//  def specExprList(prefix: String, es: List[Expr]): String = {
//    if (es == Nil) {
//      ""
//    } else {
//      s"""${push}${es.map(i => s"${newLine()}${prefix} (${i.pos}:${pp(i)});").mkString}${pop()}${newLine()}"""
//    }
//  }
//
//  def pp(x: Const): String = {
//    s"(${x.pos}:" +  (x match {
//      case IntConst(v)  => v.toString()
//      case BoolConst(v) => v.toString()
//      case NullConst(t) => s"${pp(t)}.null"
//      case NullTid()    => s"Tid.null"
//      case MoverConst(m) => pp(m)
//    }) + ")"
//  }
//
//  def pp(x : Location) : String = {
//    s"(${x.pos}:" + (x match {
//      case VarAccess(name) => name
//      case FieldAccess(v, name) => s"${pp(v)}.${pp(name)}"
//      case ArrayAccess(l, index) => s"${pp(l)}[${pp(index)}]"
//      case Old(l) => s"old(${pp(l)})"
//    }) + ")"
//  }
//
//  def pp(x: Expr): String = {
//    s"(${x.pos}:" + (x match {
//      case ConstExpr(c)                                                      => pp(c)
//      case BinaryExpr(lhs, rhs, op)                                          => s"(${pp(lhs)}${pp(op)}${pp(rhs)})"
//      case UnaryExpr(expr, op)                                               => s"${pp(op)}(${pp(expr)})"
//      case Cond(p, tt, ff) if tt.isInstanceOf[Cond] || ff.isInstanceOf[Cond] => s"(${pp(p)}${push}${newLine()} ? ${pp(tt)}${newLine()} : ${pp(ff)}${pop()})"
//      case Cond(p, tt, ff)                                                   => s"(${pp(p)} ? ${pp(tt)} : ${pp(ff)})"
//      case ForAll(decls, pred, triggers)                                               => {
//        val ts = triggers.map(_.mkString("{ ", ", ", " }")).mkString(" ")
//        s"forall ${decls.map(pp(_)).mkString(",")} ${ts} :: ${pp(pred)}"
//      }
//      case x: Location                                                       => pp(x)
//      case x: PrimitiveFunction                                              => pp(x)
//      case LoweredExpr(e, original)                                          => s"${pp(e)} /* lowered ${pp(original)} */"
//    }) + ")"
//  }
//
//
//  def pp(x: BinaryOp): String = {
//    s"(${x.pos}:" + (x match {
//      case Add()     => "+"
//      case Sub()     => "-"
//      case Mul()     => "*"
//      case Div()     => "/"
//      case Mod()     => "%"
//      case EQ()      => "=="
//      case LT()      => "<"
//      case GT()      => ">"
//      case LE()      => "<="
//      case GE()      => ">="
//      case NE()      => "!="
//      case And()     => "&&"
//      case Or()      => "||"
//      case Implies() => "==>"
//    }) + ")"
//
//  }
//  def pp(x: UnaryOp): String = {
//    s"(${x.pos}:" + (x match {
//      case Neg()     => "-"
//      case Not()     => "!"
//      case Paren() => ""
//    }) + ")"
//  }
//
//  def pp(x: PrimitiveFunction): String = {
//    s"(${x.pos}:" + (x match {
//      case Length(a)             => s"${pp(a)}.length"
//      case Holds(e, tid)         => s"holds(${pp(e)}, ${pp(tid)})"
//      case IsLocal(e, tid)       => s"isLocal(${pp(e)}, ${pp(tid)})"
//      case IsShared(e)           => s"isShared(${pp(e)})"
//      case NextCASSucceeds(l, t) => s"casOK(${pp(l)}, ${pp(t)})"
//      case Rand()                => "*"
//    }) + ")"
//  }
//
//  def pp(x: Type): String = {
//    s"(${x.pos}:" + (x match {
//      case IntType()                             => "int"
//      case BoolType()                            => "bool"
//      case VoidType()                            => "void"
//      case ClassType(x)                          => pp(x)
//      case TidType()                             => "Tid"
//      case MoverType()                           => "Mover"
//      case BoogieType(name)                      => name
//      case ArrayType(enclosing, ident, thisName) => s"[${enclosing}.${ident}{${pp(thisName)}}]"
//    }) + ")"
//  }
//}
//
//object PrettyPrintWithPosition {
//  def pp(x: Program): String = {
//    new PrettyPrintWithPosition().pp(x)
//  }
//
//  def pp(x: Expr): String = {
//    (new PrettyPrintWithPosition()).pp(x)
//  }
//
//  def pp(x: Stmt): String = {
//    (new PrettyPrintWithPosition()).pp(x)
//  }
//
//  def pp(x: Type): String = {
//    (new PrettyPrintWithPosition()).pp(x)
//  }
//
//  def pp(x: Mover): String = {
//    (new PrettyPrintWithPosition()).pp(x)
//  }
//
//  def pp(x: Spec): String = {
//    (new PrettyPrintWithPosition()).pp(x)
//  }
//  def pp(x: Spec, path: Int): String = {
//    (new PrettyPrintWithPosition()).pp(x, path)
//  }
//}
//
