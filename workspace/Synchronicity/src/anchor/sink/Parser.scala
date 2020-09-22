package anchor.sink

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import anchor.util._
import anchor.transforms._
import AST.pos

class Parser extends JavaTokenParsers {

  val _class: Parser[String] = "class\\b".r
  val _object: Parser[String] = "object\\b".r
  val _guarded_by: Parser[String] = "guarded_by\\b".r
  val _write_guarded_by: Parser[String] = "write_guarded_by\\b".r
  val _public: Parser[String] = "public\\b".r
  val _void: Parser[String] = "void\\b".r
  val _int: Parser[String] = "int\\b".r
  val _bool: Parser[String] = "bool\\b".r
  val _Tid: Parser[String] = "Tid\\b".r
  val _isLocal: Parser[String] = "isLocal\\b".r
  val _isshared: Parser[String] = "isShared\\b".r
  val _Mover: Parser[String] = "Mover\\b".r
  val _permission: Parser[String] = "permission\\b".r
  val _goesWrong: Parser[String] = "goesWrong\\b".r
  val _holds: Parser[String] = "holds\\b".r
  val _share: Parser[String] = "share\\b".r
  val _if: Parser[String] = "if\\b".r
  val _else: Parser[String] = "else\\b".r
  val _while: Parser[String] = "while\\b".r
  val _synchronized: Parser[String] = "synchronized\\b".r
  val _acquire: Parser[String] = "acquire\\b".r
  val _release: Parser[String] = "release\\b".r
  val _return: Parser[String] = "return\\b".r
  val _break: Parser[String] = "break\\b".r
  val _continue: Parser[String] = "continue\\b".r
  val _cas: Parser[String] = "cas\\b".r
  val _nextSpecStep: Parser[String] = "nextSpecStep\\b".r
  val _new: Parser[String] = "new\\b".r
  val _null: Parser[String] = "null\\b".r
  val _true: Parser[String] = "true\\b".r
  val _false: Parser[String] = "false\\b".r
  val _forall: Parser[String] = "forall\\b".r
  val _exists: Parser[String] = "exists\\b".r
  val _requires: Parser[String] = "requires\\b".r
  val _modifies: Parser[String] = "modifies\\b".r
  val _whenever: Parser[String] = "whenever\\b".r
  val _volatile: Parser[String] = "volatile\\b".r
  val _abaFree: Parser[String] = "noABA\\b".r
  val _yield: Parser[String] = "yield\\b".r
  val _readonly: Parser[String] = "readonly\\b".r
  val _invariant: Parser[String] = "invariant\\b".r
  val _decreases: Parser[String] = "decreases\\b".r
  val _ensures: Parser[String] = "ensures\\b".r
  val _yields_as: Parser[String] = "yields_as\\b".r
  val _havoc: Parser[String] = "havoc\\b".r
  val _threadlocal: Parser[String] = "threadlocal\\b".r
  val _assume: Parser[String] = "assume\\b".r
  val _assumeNonBlocking: Parser[String] = "assume-non-blocking\\b".r
  val _assert: Parser[String] = "assert\\b".r
  val _old: Parser[String] = "old\\b".r
  val _for: Parser[String] = "for\\b".r
  val _R: Parser[String] = "R\\b".r
  val _L: Parser[String] = "L\\b".r
  val _B: Parser[String] = "B\\b".r
  val _N: Parser[String] = "N\\b".r
  val _E: Parser[String] = "E\\b".r
  val _boogie: Parser[String] = "## .*\n".r
  val _length: Parser[String] = "length\\b".r
  val _array: Parser[String] = "array\\b".r
  val _nocheck : Parser[String] = "nocheck\\b".r
  val _as : Parser[String] = "as\\b".r

  val reserved: Parser[String] = _class |||
    _object |||
    _guarded_by |||
    _write_guarded_by |||
    _public |||
    _void |||
    _int |||
    _bool |||
    _share |||
    _if ||| _forall ||| _exists |||
    _else |||
    _while |||
    _synchronized ||| _acquire ||| _release ||| _goesWrong |||
    _return |||
    _break |||
    _continue |||
    _cas |||
    _new |||
    _null |||
    _true |||
    _false |||
    _whenever |||
    _yield |||
    _readonly |||
    _invariant ||| _decreases ||| _volatile ||| _abaFree ||| _nextSpecStep |||
    _havoc ||| _as |||
    _assume |||
    _assert ||| _for ||| _Mover |||
    _isLocal |||
    _holds ||| _length ||| _array ||| _permission  |||
    _R ||| _L ||| _B ||| _N ||| _E ||| _boogie ||| _Tid ||| _old ||| _ensures ||| _yields_as ||| _nocheck

  val Ident: Parser[String] = not(reserved) ~> ident

  val VarAccessP: Parser[VarAccess] = positioned {
    Ident ^^ { x => VarAccess(x) }
  }

  val ClassTypeP: Parser[ClassType] = positioned {
    Ident ^^ { x => ClassType(x) }
  }

  val ArrayIdentP: Parser[(String, String)] = {
    val a1 = Ident ^^ { c => (currentClass, c) }
    val a2 = Ident ~ "." ~ Ident ^^ { case c ~ _ ~ i => (c, i) }
    a1 ||| a2
  }

  val ArrayTypeP: Parser[ArrayType] = positioned {
    val expl = "[" ~ ArrayIdentP ~ "{" ~ VarAccessP ~ "}" ~ "]" ^^ { case _ ~ x ~ _ ~ i ~ _ ~ _ => ArrayType(x._1, x._2, i) }
    val impl = "[" ~ ArrayIdentP ~ "]" ^^ { case _ ~ x ~ _ => ArrayType(x._1, x._2, VarAccess("this")) }
    impl ||| expl
  }

  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def ProgramP: Parser[Program] = positioned {
    phrase(rep(ClassDeclP) ~ rep(VarDeclP <~ ";")) ^^ { case x ~ d  => Program(x, d, List(), new Library(Map())) }
  }

  private var currentClass: String = _

  val triggersP : Parser[List[List[Expr]]] = {
    rep("{" ~> rep(ExprP) <~ "}")
  }

  val ClassInvariantP : Parser[ClassInvariant] = positioned {
    _invariant ~> triggersP ~ ExprP <~ ";" ^^ { case triggers ~ expr => ClassInvariant(expr, triggers) }
  }


  def ClassDeclP: Parser[ClassDecl] = positioned {
    _class ~ (Ident ^^ { i => currentClass = i; i }) ~ "{" ~ rep(ArrayDeclP) ~ rep(FieldDeclP) ~ rep(ClassInvariantP) ~ rep(MethodDeclP) ~ "}" ^^ {
      case _ ~ i ~ _ ~ arrays ~ fields ~ invariants ~ methods ~ _ =>
        val cd = ClassDecl(i,
          arrays,
          fields,
          methods,
          invariants)
        cd
    }
  }

  def ArrayDeclP: Parser[ArrayDecl] = positioned {
    _array ~ (Ident ^^ { i => currentLoc = s"athis[index]"; i }) ~ "=" ~ BaseTypeP ~ "[" ~ SpecP ~ "]" ^^ { case _ ~ i ~ _ ~ b ~ _ ~ spec ~ _ => ArrayDecl(i, b, "index", spec) }
  }

  def MoverP : Parser[Mover] = {
    val b: Parser[Mover] = _B ^^ { _ => B() }
    val l: Parser[Mover] = _L ^^ { _ => L() }
    val r: Parser[Mover] = _R ^^ { _ => R() }
    val n: Parser[Mover] = _N ^^ { _ => N() }
    val e: Parser[Mover] = _E ^^ { _ => E() }
    (b | l | r | n | e)
  }

  def MoverConstP: Parser[MoverConst] = positioned {
    MoverP ^^ { case m => MoverConst(m) }
  }

  def SpecP: Parser[Spec] = positioned {
    ExprP ~ opt("!") ~ rep(_yields_as ~> ExprP) ^^ { case e ~ blocking ~ yields => Spec(e, blocking != None, yields) }
  }

  private var currentLoc: String = _

  def PredefinedSyncDisciplinesP: Parser[Expr] = positioned {
    val gb = _guarded_by ~ LocationP ^^ { case _ ~ l => parseExpr(s"(holds (${PrettyPrint.pp(l)})) ? B:E") }
    val wgb = _write_guarded_by ~ LocationP ^^ { case _ ~ l => parseExpr(s"(holds (${PrettyPrint.pp(l)})) ? (B#N):(N#E)") }
    val vol = _whenever ^^ { _ => parseExpr("N") }
    val ro = _readonly ^^ { _ => parseExpr("B#E") }
    val tl = _threadlocal ^^ { _ => parseExpr("(isLocal(this)) ? B : E") }
    val cas = _cas ^^ { _ => parseExpr(s"(casOK(${currentLoc}) ? R#N : N#E)") }

    // (this.nx == tid ? R#N : (this.nx == Tid.null) ? N#N : N#E) !

    gb | wgb | vol | ro | tl | cas
  }

  def FieldModifierP : Parser[FieldModifier] = positioned {
    _volatile ^^ { case _ => VolatileModifier() } |
    _abaFree  ^^ { case _ => ABAFreeModifier() }
  }

  def FieldDeclP: Parser[FieldDecl] = positioned {
    rep(FieldModifierP) ~ TypeP ~ (Ident ^^ { i => currentLoc = s"this.${i}"; i }) ~ SpecP ~ ";" ^^ {
      case v ~ t ~ i ~ s ~ _ => FieldDecl(t, i, s, v)
    }
  }


  def MethodSpecP: Parser[ExplicitMethodSpec] = positioned {

    val t = rep(_modifies ~> LocationP <~ ";") ~ rep(_ensures ~> ExprP <~ ";")
    val trans = "{" ~> t <~ "}*" ^^ { case t => Transaction(true, t._1, t._2) } |||
      t ^^ { case t => Transaction(false, t._1, t._2) }

    rep(_requires ~> ExprP <~ ";") ~
      rep(VarDeclP <~ ";") ~
      repsep(trans, _yield ~ ";") ^^ { case r ~ v ~ t => ExplicitMethodSpec(r, v, t) }
  }

  def ensureNonEmptySpec(spec: ExplicitMethodSpec) : ExplicitMethodSpec = {
    if (spec.transactions == List.empty) {
      pos(ExplicitMethodSpec(spec.requires, List.empty, List(pos(Transaction(false, List.empty, List(pos(ConstExpr(   pos(BoolConst(true),spec.pos)), spec.pos))), spec.pos))), spec.pos)
    } else {
      spec
    }
  }

  def MethodDeclP: Parser[MethodDecl] = positioned {
    MethodSpecP ~ opt(_public) ~ TypeP ~ Ident ~ "(" ~ repsep(VarDeclP, ",") ~ ")" ~ BlockP ^^ {
      case spec ~ a ~ t ~ i ~ _ ~ params ~ _ ~ stmt => {
          val r = t match {
            case IntType() => Some(pos(ConstExpr(pos(IntConst(-1), stmt.pos)), stmt.pos))
            case TidType() => Some(pos(ConstExpr(pos(IntConst(-1), stmt.pos)), stmt.pos))
            case VoidType() => None
            case BoolType() => Some(pos(ConstExpr(pos(BoolConst(false), stmt.pos)), stmt.pos))
            case BoogieType(name) => None
            case MoverType() => None
            case r: RefType => Some(pos(ConstExpr(pos(NullConst(r), stmt.pos)), stmt.pos))
            case r: CollectionType => Some(pos(ConstExpr(pos(EmptyCollectionConst(r), stmt.pos)), stmt.pos))
            case _ : TypeVar => Errors.fail("parser", "Bad return Type")
          }

        val fullSpec = if (t != VoidType()) {
          pos(ExplicitMethodSpec(spec.requires, spec.vars :+ pos(VarDecl(pos(t, spec.pos), "$result"), spec.pos), spec.transactions), spec.pos)
        } else {
          spec
        }
          MethodDecl(a.isDefined, t, i, params, fullSpec, pos(Block(stmt.label, stmt.body :+ pos(Return(r, true), stmt.pos)), stmt))
        }
    }
  }

  def BaseTypeP: Parser[Type] = positioned {
    val voidType = _void ^^ { _ => VoidType() }
    val intType = _int ^^ { _ => IntType() }
    val boolType = _bool ^^ { _ => BoolType() }
    val tidType = _Tid ^^ { _ => TidType() }
    val classType = ClassTypeP ^^ { x => x }
    val moverType = _Mover ^^ { _ => MoverType() }
    voidType | intType | boolType | classType | tidType | moverType
  }

  def TypeP: Parser[Type] = positioned {
    BaseTypeP ||| ArrayTypeP
  }

  def VarDeclP: Parser[VarDecl] = positioned {
    TypeP ~ Ident ^^ { case t ~ i => VarDecl(t, i) }
  }
  def CloseParenP: Parser[Positional] = positioned {
    "}" ^^ { _ => new Positional() {} }
  }

  def BlockP: Parser[Block] = positioned {
    opt(Ident ~ ":" ^^ { case i ~ _ => i }) ~ "{" ~ StmtsP ~ "}" ^^ { case n ~ _ ~ i ~ _ => Block(n, i) }
  }

  def StmtsP: Parser[List[Stmt]] = {
    rep(StmtHelper) ^^ {
      ss =>
        ss.flatMap(s =>
          s match {
            case Block(None, body) => body
            case _                     => List(s)
          }
        )
    }
  }

  def InvariantP: Parser[Expr] = positioned {
    _invariant ~> ExprP <~ ";"
  }

  def InvOrDecreaseP: Parser[(List[Expr],List[Expr])] = {
    val one =
      (_invariant ~> ExprP <~ ";" ^^ { case x => (List[Expr](x), List[Expr]()) }) |
      (_decreases ~> ExprP <~ ";" ^^ { case x => (List[Expr](), List[Expr](x)) })

    rep(one) ^^ { case x => {
      val (f,s) = x.unzip
      (f.flatten, s.flatten)
    }}
  }

  // Stmt List Builders

  def Nil() = Block(None, List())

  def Seq(p: Stmt, q: Stmt) = Cons(p, Cons(q, Nil()))

  def Seq(p: Stmt, q: Stmt, r: Stmt) = Cons(p, Cons(q, Cons(r, Nil())))

  def Seq(o: Stmt, p: Stmt, q: Stmt, r: Stmt) = Cons(o, Cons(p, Cons(q, Cons(r, Nil()))))

  def Cons(p: Stmt, b: Block) = {
    p match {
      case Block(label, q) => Block(label, q ::: b.body)
      case q                  => Block(b.label, q :: b.body)
    }
  }

  def Append(b: Block, p: Stmt) = {
    p match {
      case Block(label, q) => Block(b.label,  b.body ++ q)
      case q           => Block(b.label,   b.body :+ q)
    }
  }

  ///

  case class StmtVar(s: Stmt, v: VarAccess)

  def StmtHelper: Parser[Stmt] = {

    val decl: Parser[Stmt] = positioned {
      VarDeclP ~ ";" ^^ { case t ~ _ => VarDeclStmt(t) }
    }

    val declOrUse: Parser[StmtVar] = {
      val dec: Parser[StmtVar] = {
        VarDeclP ^^ { x => StmtVar(pos(VarDeclStmt(x), x.pos), pos(VarAccess(x.name), x.pos)) }
      }
      val use: Parser[StmtVar] = {
        VarAccessP ^^ { x => StmtVar(pos(Nil(), x.pos), x) }
      }
      dec | use
    }

    val varOrConstWithLocal: Parser[StmtVar] = {
      val aConst: Parser[StmtVar] = {
        ConstP ^^ { x => {
          val name = acme.scala.Util.fresh("tmp")
          StmtVar(Seq(pos(VarDeclStmt(pos(VarDecl(pos(DeepCopyWithPositions(x.typeOf()), x.pos), name), x.pos)), x.pos), pos(Assign(pos(VarAccess(name), x.pos), pos(ConstExpr(x), x.pos)), x.pos)), pos(VarAccess(name), x.pos))
        }
        }
      }
      val aVar: Parser[StmtVar] = {
        VarAccessP ^^ { x => StmtVar(pos(Nil(), x.pos), x) }
      }
      aConst | aVar
    }


    val varOrConst: Parser[VarOrConst] = positioned {
        val c = ConstP ^^ { case x => ConstExpr(x) }
        c | VarAccessP
    }

    val AssignP: Parser[Stmt] = positioned {
      rep1sep(declOrUse, ",") ~ "=" ~ rep1sep(ExprP, ",")  ^^ { case stmtVars ~ eq ~ es =>
        val stmtSeq = stmtVars.map(_.s).foldRight(Nil()) { (sv, r) => Cons(sv, r) }
        val lhss = stmtVars.map(_.v)
        val rhss = es
        Cons(stmtSeq, Seq(pos(Assign(lhss, rhss), es(0).pos), Nil()))
      }
    }

    val MovesAsP = {
      _as ~ MoverP ^^ { case _ ~ m => m }
    }

    val ReadP: Parser[Stmt] = positioned {
      declOrUse ~ ":=" ~ VarAccessP ~ "." ~ Ident ~ opt(MovesAsP) ^^ { case StmtVar(stmt, x) ~ _ ~ v2 ~ _ ~ i ~ m => Seq(stmt, pos(Read(x, v2, i, m), stmt.pos)) }
    }
    val AReadP: Parser[Stmt] = positioned {
      declOrUse ~ ":=" ~ VarAccessP ~ "[" ~ varOrConstWithLocal ~ "]"  ^^ { case StmtVar(stmt, x) ~ _ ~ v2 ~ _ ~ StmtVar(stmt2,i) ~ _ => Seq(stmt, pos(stmt2, x.pos), pos(ARead(x, v2, i), x.pos)) }
    }
    val AllocP: Parser[Stmt] = positioned {
      declOrUse ~ "=" ~ _new ~ ClassTypeP ~ "(" ~ ")"  ^^ { case StmtVar(stmt, x) ~ _ ~ _ ~ c ~ _ ~ _ => Seq(stmt, pos(Alloc(x, c), stmt.pos)) }
    }
    val AAllocP: Parser[Stmt] = positioned {
      declOrUse ~ "=" ~ _new ~ ArrayTypeP ~ "(" ~ varOrConst ~ ")"  ^^ { case StmtVar(stmt, x) ~ _ ~ _ ~ c ~ _ ~ size ~ _ => Seq(stmt, pos(AAlloc(x, c, size), stmt.pos)) }
    }
    val OptCASP: Parser[Stmt] = positioned {
      declOrUse ~ "=" ~ VarAccessP ~ "." ~ Ident ~ "@" ~ varOrConst ~ ":~" ~ varOrConst  ^^ { case StmtVar(stmt, x) ~ _ ~ i ~ _ ~ f ~ _ ~ exp ~ _ ~ upd => Seq(stmt, pos(CAS(x, i, f, exp, upd), stmt.pos)) }
    }
    val InvokeP: Parser[Stmt] = positioned {
      opt(declOrUse ~ "=" ^^ { case t ~ _ => t }) ~ VarAccessP ~ "." ~ Ident ~ "(" ~ repsep(ExprP, ",") ~ ")" ~ rep(this.InvariantP)  ^^ {
        case dest ~ obj ~ _ ~ method ~ _ ~ args ~ _ ~ invariants =>
          dest match {
            case None                   => Invoke(obj, method, args, None, invariants)
            case Some(StmtVar(stmt, x)) => Seq(stmt, pos(Invoke(obj, method, args, Some(x), invariants), stmt.pos))
          }
      }
    }

    val WriteP: Parser[Write] = positioned {
      VarAccessP ~ "." ~ Ident ~ ":=" ~ varOrConst ~ opt(MovesAsP) ^^ { case v ~ _ ~ i ~ _ ~ e ~ m =>  Write(v, i, e, m) }
    }

    val LocalWritesP : Parser[Stmt] = positioned {
      WriteP ~ "," ~ rep1sep(WriteP, ",") ^^ { case w ~ _ ~ writes => LocalWrites(w::writes) }
    }

    val AWriteP: Parser[Stmt] = positioned {
      VarAccessP ~ "[" ~ varOrConstWithLocal ~ "]" ~ ":=" ~ varOrConst ^^ { case v ~ _ ~ StmtVar(stmt,i) ~ _ ~ _ ~ e => Seq(pos(stmt, v.pos), pos(AWrite(v, pos(i, stmt.pos), e), v.pos)) }
    }

    val SimpleStmtNoSemi = (AssignP ||| ReadP ||| AReadP ||| AllocP ||| AAllocP ||| OptCASP ||| InvokeP ||| WriteP ||| AWriteP ||| LocalWritesP)
    val SimpleStmt = SimpleStmtNoSemi <~ ";"

    val IfP: Parser[Stmt] = positioned {
      _if ~ "(" ~ ExprP ~ ")" ~ BlockP ~ _else ~ BlockP ^^ { case _ ~ _ ~ e ~ _ ~ t ~ _ ~ f => If(e, t, f) }
    }
    val IfNoElseP: Parser[Stmt] = positioned {
      _if ~ "(" ~ ExprP ~ ")" ~ BlockP <~ not(_else) ^^ { case _ ~ _ ~ e ~ _ ~ t => If(e, t, pos(Block(None, List[Stmt]()), e)) }
    }

    val SyncP: Parser[Stmt] = positioned {
      _synchronized ~ "(" ~ VarAccessP ~ ")" ~ "{" ~ StmtsP ~ CloseParenP ^^ { case _ ~ _ ~ i ~ _ ~ _ ~ s ~ cp => Sync(i, pos(Block(None, s), if (s.isEmpty) i.pos else s.head.pos), cp.pos) }
    }
    val AcquireP: Parser[Stmt] = positioned {
      _acquire ~ "(" ~ VarAccessP ~ ")" ~ ";" ^^ { case _ ~ _ ~ i ~ _ ~ _ => Acquire(i) }
    }
    val ReleaseP: Parser[Stmt] = positioned {
      _release ~ "(" ~ VarAccessP ~ ")" ~ ";" ^^ { case _ ~ _ ~ i ~ _ ~ _ => Release(i) }
    }
    val WhileP: Parser[Stmt] = positioned {
      _while ~ "(" ~ ExprP ~ ")" ~ InvOrDecreaseP ~ BlockP ^^ { case _ ~ _ ~ e ~ _ ~ ids ~ t => While(e, t, ids._1, ids._2) }
    }

    val ForP: Parser[Stmt] = positioned {
      _for ~ "(" ~ SimpleStmt ~ ExprP ~ ";" ~ SimpleStmtNoSemi ~ ")" ~ InvOrDecreaseP ~ BlockP ^^ { case _ ~ _ ~ init ~ e ~ _ ~ incr ~ _ ~ ids ~ t => {
        Seq(init, pos(While(e, pos(Append(t, incr), t.pos), ids._1, ids._2), e.pos))
      }
      }}

    val BreakP: Parser[Stmt] = positioned {
      _break ~> opt(Ident) <~ ";" ^^ { s => Break(s) }
    }
    val ReturnP: Parser[Stmt] = positioned {
      _return ~ opt(varOrConst) ~ ";" ^^ { case _ ~ x ~ _ => Return(x, false) }
    }
    val AssumeP: Parser[Stmt] = positioned {
      _assume ~ ExprP ~ ";" ^^ { case _ ~ x ~ _ => Assume(x) }
    }
    val AssertP: Parser[Stmt] = positioned {
      _assert ~ ExprP ~ ";" ^^ { case _ ~ x ~ _ => Assert(x) }
    }
    val InvariantP: Parser[Stmt] = positioned {
      _invariant ~ ExprP ~ ";" ^^ { case _ ~ x ~ _ => Invariant(x) }
    }
    val YieldP: Parser[Stmt] = positioned {
      _yield ~ repsep(_ensures ~> ExprP, ";") ~ ";" ^^ { case _ ~ ensures ~ _ => Yield(ensures) }
    }

    val BoogieP: Parser[Stmt] = positioned {
      _boogie ^^ { x => BoogieCode(x.substring(2)) }
    }

    val annotationP : Parser[Stmt] = positioned {
      _nocheck ~> StmtHelper ^^ { case s => NoReductionCheck(s) }
    }

    SimpleStmt | BlockP | IfP | IfNoElseP | SyncP | AcquireP | ReleaseP | WhileP | ForP | BreakP | ReturnP | decl | YieldP | AssumeP | AssertP | BoogieP | InvariantP | annotationP
  }

  def ExprP: Parser[Expr] = {
    val valueExpr = positioned {
      ConstP ^^ { x => ConstExpr(x) }
    }
//    def locationOrOldLocation : Parser[Location] = positioned {
//      LocationP | (_old ~ "(" ~ locationOrOldLocation ~ ")" ^^ { case _ ~ _ ~ l ~ _ => Old(l) })
//    }
    val parenExpr = positioned {
      "(" ~ ExprP ~ ")" ^^ { case _ ~ x ~ _ => x /* UnaryExpr(x, Paren()) */ } |
        (_old ~> "(" ~> ExprP <~ ")" ^^ { case e => Old(e)} )
}
    val primitiveFunction: Parser[PrimitiveFunction] =
      positioned {
        (_isLocal ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => IsLocal(x, pos(VarAccess("tid"), x)) }) |
          (_holds ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => Holds(x, pos(VarAccess("tid"), x)) }) |
          (_permission ~ "(" ~ ExprP ~ opt("," ~> ExprP) ~ ")" ^^ { case _ ~ _ ~ x ~ v ~ _ => MoverPermission(x, v) }) |
          (_goesWrong ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ e ~ _ => GoesWrong(e)}) |
          (_isshared ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => IsShared(x) }) |
          (LocationP ~ "." ~ _length ^^ { case x ~ _ ~ _ => Length(x) }) |
          (_nextSpecStep ~> "(" ~> wholeNumber <~ ")") ^^ { case n => NextSpecStep(Integer.parseInt(n))}

      }

    val factor: Parser[Expr] = positioned {
      rep(UnaryOpP) ~ (PredefinedSyncDisciplinesP | valueExpr | primitiveFunction | parenExpr | LocationP) ^^ { case ops ~ e => ops.foldRight(e)({ case (op, e) => UnaryExpr(e, op) }) }
    }

    def mkOp(s: String) : BinaryOp = {
      s match {
        case "*"   => Mul()
        case "/"   => Div()
        case "%"   => Mod()
        case "+"   => Add()
        case "-"   => Sub()
        case "=="  => EQ()
        case "!="  => NE()
        case "<"   => LT()
        case "<="  => LE()
        case ">"   => GT()
        case ">="  => GE()
        case "&&"  => And()
        case "||"  => Or()
        case "==>" => Implies()
      }
    }

    def mkExpr(base: Parser[Expr], ops: List[String]): Parser[Expr] = positioned {
      val operator : Parser[BinaryOp] = positioned {
        ops.map( a => a ^^ { case _ => mkOp(a) }).reduce( (a,b) => a ||| b)
      }
      val op : Parser[(Expr,Expr) => Expr]=  {
        (operator ^^ { case op => (a: Expr, b: Expr) => pos(BinaryExpr(a, b, op), op) })
      }
      chainl1(base, op)
    }

    val term = mkExpr(factor, List("*", "/", "%"))
    val expr = mkExpr(term, List("+", "-"))
    val compExpr = mkExpr(expr, List("==", "!=", "<=", "<", ">", ">="))
    val andExpr = mkExpr(compExpr, List("&&"))
    val orExpr = mkExpr(andExpr, List("||"))
    val impliesExpr = mkExpr(orExpr, List("==>"))

    def quantifierP() : Parser[Quantifier] = {
      _forall ^^ { case _ => ForAll() } |||
        _exists ^^ { case _ => Exists() }
    }

    val quantifiedExpr: Parser[Expr] = positioned {
      quantifierP() ~ rep1sep(VarDeclP, ",") ~ "::" ~ triggersP ~ impliesExpr ^^ { case q ~ decls ~ _ ~ triggers ~ pred => Quantified(q, decls, pred, triggers) } |||
        impliesExpr
    }


    val slashExpr: Parser[Expr] = positioned {
      val hashP = positioned {
        "#" ^^ { case _ => new NoNode() }
      }
      val op : Parser[(Expr,Expr) => Expr]= {
        (hashP ^^ { case h => (x: Expr, y: Expr) => pos(Cond(pos(VarAccess("isRead"), x.pos), x, y), h.pos) })
      }
      chainl1(quantifiedExpr, op)
    }

    val condExpr: Parser[Expr] = positioned {
      quantifiedExpr ~ "?" ~ ExprP ~ ":" ~ ExprP ^^ { case p ~ _ ~ tt ~ _ ~ ff => Cond(p, tt, ff) } |||
      slashExpr
    }

    val havoc: Parser[Expr] = positioned {
      "*" ^^ { case _ => Rand() }
    }

    positioned {
        condExpr ||| havoc
    }
  }

  def LocationP: Parser[Location] = positioned {
    VarAccessP ~ rep(
      "." ~ Ident ^^ { case _ ~ i => (x: Location) => pos(FieldAccess(x, i).asInstanceOf[Location], x) }
        | "[" ~ ExprP ~ "]" ^^ { case _ ~ i ~ _ => (x: Location) => pos(ArrayAccess(x, i).asInstanceOf[Location], x) }
    ) ^^ { case x ~ es => es.foldLeft(x.asInstanceOf[Location])((l, f) => f(l)) }
  }


  def ConstP: Parser[Const] = {
    val boolValue = {
      val trueValue = positioned {
        _true ^^ { _ => BoolConst(true) }
      }
      val falseValue = positioned {
        _false ^^ { _ => BoolConst(false) }
      }
      trueValue | falseValue
    }
    val intValue = positioned {
      wholeNumber ^^ { x => IntConst(x.toInt) }
    }
    val nullValue = positioned {
      (ClassTypeP | ArrayTypeP) ~ "." ~ _null ^^ { case c ~ _ ~ _ => NullConst(c) }
    }
    val nullTid = positioned {
      _Tid ~ "." ~ _null ^^ { case c ~ _ ~ _ => NullTid() }
    }
    boolValue | intValue | nullValue | nullTid | MoverConstP
  }

  def UnaryOpP: Parser[UnaryOp] = {
    val not = positioned {
      "!" ^^ { str => Not() }
    }
    val neg = positioned {
      "-" ^^ { str => Neg() }
    }
    not | neg
  }


  private def parseIt[T](text: String, parser : Parser[T]): T = {
    parse(parser, text) match {
      case Success(matched, _) => matched
      case Failure(msg, loc)   => Errors.error("Syntax", msg, NoNode(loc.pos)); null.asInstanceOf[T]
      case Error(msg, loc)     => Errors.error("Syntax", msg, NoNode(loc.pos)); null.asInstanceOf[T]
    }
  }

  def parse(prog: String) = parseIt(prog, ProgramP)
  def parseExpr(expr: String) = parseIt(expr, ExprP)
  def parseStmt(expr: String) = parseIt(expr, StmtHelper)
  def parseBlock(expr: String) = parseIt(expr, BlockP)
  def parseLocation(expr: String) = parseIt(expr, LocationP)
  def parseSpec(expr: String) = parseIt(expr, SpecP)
  def parseFieldDecl(expr: String) = parseIt(expr, FieldDeclP)
  def parseVarDecls(expr: String) = parseIt(expr, rep(VarDeclP <~ ";"))
}

object Parser {
  def parse(prog: String): Program = {
    new Parser().parse(prog)
  }

  def expr(expr: String): Expr = {
    new Parser().parseExpr(expr)
  }

  def stmt(s: String): Stmt = {
    new Parser().parseStmt(s)
  }

  def block(s: String): Stmt = {
    new Parser().parseBlock(s)
  }

  def location(expr: String): Location = {
    new Parser().parseLocation(expr)
  }

  def spec(expr: String): Spec = {
    new Parser().parseSpec(expr)
  }

  def fieldDecl(expr: String): FieldDecl = {
    new Parser().parseFieldDecl(expr)
  }

  def decls(expr: String): List[VarDecl] = {
    new Parser().parseVarDecls(expr)
  }

}
