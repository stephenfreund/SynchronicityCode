package anchor.lang

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.{Scanners, StdLexical}
import scala.util.parsing.input._
import anchor.util._
import AST.pos
import anchor.transforms.FixPosition

class Parser extends JavaTokenParsers {

  val _class: Parser[String] = "class\\b".r
  val _ghost: Parser[String] = "ghost\\b".r
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
  val _isfresh: Parser[String] = "isFresh\\b".r
  val _casok: Parser[String] = "casOK\\b".r
  val _commit: Parser[String] = "commit\\b".r
  val _holds: Parser[String] = "holds\\b".r
  val _share: Parser[String] = "share\\b".r
  val _moves_as: Parser[String] = "moves_as\\b".r
  val _axiom : Parser[String] = "axiom\\b".r
  val _if: Parser[String] = "if\\b".r
  val _else: Parser[String] = "else\\b".r
  val _while: Parser[String] = "while\\b".r
  val _volatile: Parser[String] = "volatile\\b".r
  val _abaFree: Parser[String] = "noABA\\b".r
  val _synchronized: Parser[String] = "synchronized\\b".r
  val _acquire: Parser[String] = "acquire\\b".r
  val _release: Parser[String] = "release\\b".r
  val _return: Parser[String] = "return\\b".r
  val _break: Parser[String] = "break\\b".r
  val _continue: Parser[String] = "continue\\b".r
  val _cas: Parser[String] = "cas\\b".r
  val _new: Parser[String] = "new\\b".r
  val _null: Parser[String] = "null\\b".r
  val _true: Parser[String] = "true\\b".r
  val _false: Parser[String] = "false\\b".r
  val _init: Parser[String] = "init\\b".r
  val _forall: Parser[String] = "forall\\b".r
  val _exists: Parser[String] = "exists\\b".r
  val _requires: Parser[String] = "requires\\b".r
  val _whenever: Parser[String] = "whenever\\b".r
  val _yield: Parser[String] = "yield\\b".r
  val _readonly: Parser[String] = "readonly\\b".r
  val _invariant: Parser[String] = "invariant\\b".r
  val _decreases: Parser[String] = "decreases\\b".r
  val _ensures: Parser[String] = "ensures\\b".r
  val _modifies: Parser[String] = "modifies\\b".r
  val _yields_as: Parser[String] = "yields_as\\b".r
  val _havoc: Parser[String] = "havoc\\b".r
  val _nextSpecStep: Parser[String] = "nextSpecStep\\b".r
  val _threadlocal: Parser[String] = "threadlocal\\b".r
  val _assume: Parser[String] = "assume\\b".r
  val _assert: Parser[String] = "assert\\b".r
  val _old: Parser[String] = "old\\b".r
  val _wait: Parser[String] = "wait\\b".r
  val _notify: Parser[String] = "notify\\b".r
  val _lock: Parser[String] = "lock\\b".r
  val _for: Parser[String] = "for\\b".r
  val _INT_MIN : Parser[String] = "INT_MIN\\b".r
  val _INT_MAX : Parser[String] = "INT_MAX\\b".r
  val _I: Parser[String] = "I\\b".r
  val _R: Parser[String] = "R\\b".r
  val _L: Parser[String] = "L\\b".r
  val _B: Parser[String] = "B\\b".r
  val _N: Parser[String] = "N\\b".r
  val _E: Parser[String] = "E\\b".r
  val _boogie: Parser[String] = "## .*\n".r
  val _length: Parser[String] = "length\\b".r
  val _array: Parser[String] = "array\\b".r
  val _nocheck : Parser[String] = "nocheck\\b".r
  val _set : Parser[String] = "Set\\b".r
  val _seq : Parser[String] = "Seq\\b".r
  val _pair : Parser[String] = "Pair\\b".r
  val _multiset : Parser[String] = "Multiset\\b".r
  val _map : Parser[String] = "Map\\b".r

  val reserved: Parser[String] = _class |
    _object | _set | _seq | _multiset | _map | /* _pair | */ _ghost | _axiom |
    _guarded_by |
    _write_guarded_by |
    _void |
    _int |
    _bool |
    _share |
    _if | _forall | _exists |
    _else |
    _while | _modifies |
    _synchronized | _acquire | _release | _lock | _nextSpecStep |
    _return |
    _break |
    _continue |
    _cas |
    _new |
    _null |
    _true |
    _false |
    _whenever | _init |
    _yield |
    _readonly |
    _invariant | _decreases | _commit |
    _havoc |
    _assume |
    _assert | _for |
    _isLocal |
    _holds | _length | _array | _casok | _wait | _notify |
    _I | _R | _L | _B | _N | _E | _boogie | _Tid | _old | _ensures | _yields_as | _abaFree | _INT_MIN | _INT_MAX | _nocheck | _volatile | _moves_as | _public

  val typeVarIdent : Parser[String] = {
    "#[A-Za-z0-9_]+".r
  }

  val Ident: Parser[String] = {
    (not(reserved) ~> ident) |||
    "isRead()" ^^ { _ => "isRead" } |||
    "\\result" ^^ { _ => "\\result" }

  }

  val VarAccessP: Parser[VarAccess] = positioned {
    Ident ^^ { x => VarAccess(x) }
  }

  val ClassTypeP: Parser[ClassType] = positioned {
    Ident ^^ { x => ClassType(x) }
  }

  val TypeVarP: Parser[TypeVar] = positioned {
    typeVarIdent ^^ { case x => TypeVar(x) }
  }

  val ArrayIdentP: Parser[(String, String)] = {
    Ident ~ opt("." ~> Ident) ^^ {
      case c ~ None => (currentClass, c)
      case c ~ Some(i) => (c, i)
    }
  }

  val ArrayTypeP: Parser[ArrayType] = positioned {
    "[" ~> ArrayIdentP ~ opt("{" ~> VarAccessP <~ "}") <~ "]" ^^ {
      case x ~ Some(i) => ArrayType(x._1, x._2, i)
      case x ~ None  => ArrayType(x._1, x._2, VarAccess("this"))
    }

  }

   protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r


  def ProgramP: Parser[Program] = positioned {
    phrase(rep(_axiom ~> ExprP <~ ";") ~ rep(ClassDeclP) ~ rep(VarDeclP <~ ";")) ^^ { case a ~ x ~ d => Program(x, d, a) }
  }

  private var currentClass: String = _

  val ClassInvariantP : Parser[ClassInvariant] = positioned {
    _invariant ~> triggersP ~ ExprP ^^ { case triggers ~ expr => ClassInvariant(expr, triggers) }
  }

  def ClassDeclP: Parser[ClassDecl] = positioned {
    _class ~ (Ident ^^ { i => currentClass = i; i }) ~ OpenBraceP ~ rep(ArrayDeclP ||| FieldDeclP ||| (ClassInvariantP <~ ";") ||| MethodDeclP ||| ConstructorDeclP) ~ CloseBraceP ^^ {
      case _ ~ name ~ obp ~ elems ~ cbp => {
        val constructors = elems.filter(_.isInstanceOf[ConstructorDecl]).map(_.asInstanceOf[ConstructorDecl])
        if (constructors.length > 1) Errors.error("Syntax", "Only one constructor permitted", constructors(1))
        ClassDecl(name,
          elems.filter(_.isInstanceOf[ArrayDecl]).map(_.asInstanceOf[ArrayDecl]),
          elems.filter(_.isInstanceOf[FieldDecl]).map(_.asInstanceOf[FieldDecl]),
          elems.filter(_.isInstanceOf[MethodDecl]).map(_.asInstanceOf[MethodDecl]),
          if (constructors.length > 0) constructors(0) else {
            pos(ConstructorDecl(false, name, Nil, pos((ExplicitMethodSpec(Nil,Nil,Nil)), obp), pos(Block(None, List(pos(Return(None, true), cbp))), obp)), obp)
          },
          elems.filter(_.isInstanceOf[ClassInvariant]).map(_.asInstanceOf[ClassInvariant]))
      }
    }
  }

  def ArrayDeclP: Parser[ArrayDecl] = positioned {
    _array ~ (Ident ^^ { i => currentLoc = s"athis[index]"; i }) ~ "=" ~ BaseTypeP ~ "[" ~ _moves_as ~ SpecP ~ "]" ^^ { case _ ~ i ~ _ ~ b ~ _ ~ _~ spec ~ _ => ArrayDecl(i, b, "index", spec) }
  }

  def MoverConstP: Parser[MoverConst] = positioned {
    val i: Parser[Mover] = _I ^^ { _ => I() }
    val b: Parser[Mover] = _B ^^ { _ => B() }
    val l: Parser[Mover] = _L ^^ { _ => L() }
    val r: Parser[Mover] = _R ^^ { _ => R() }
    val n: Parser[Mover] = _N ^^ { _ => N() }
    val e: Parser[Mover] = _E ^^ { _ => E() }
    (i | b | l | r | n | e) ^^ { case m => MoverConst(m) }
  }

  def SpecP: Parser[MoverSpec] = positioned {
    ExprP ~ opt("!") ~ rep(_yields_as ~> ExprP) ^^ { case e ~ blocking ~ yields => MoverSpec(e, blocking != None, yields) }
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
    _abaFree  ^^ { case _ => ABAFreeModifier() } |
    _ghost ^^ { case _ => GhostModifier() }
  }

  def FieldDeclP: Parser[FieldDecl] = positioned {
    rep(FieldModifierP) ~ TypeP ~ (Ident ^^ { i => currentLoc = s"this.${i}"; i }) ~ opt(_moves_as ~> SpecP) ~ ";" ^^ {
       case v ~ t ~ i ~ s ~ _ => s match {
         case Some(s) => FieldDecl(t, i, s, v)
         case None    => FieldDecl(t, i, pos(MoverSpec(parseExpr("E"), false, Nil), t.pos), v)
       }
    }
  }

  def MethodSpecP: Parser[ExplicitMethodSpec] = positioned {

    val t = rep(_modifies ~> rep1sep(ExprP, ",") <~ ";") ~ rep(_ensures ~> ExprP <~ ";")
    val trans = "{" ~ t ~ "}" ~ opt("*") ^^ { case _ ~ t ~ _ ~ o => Transaction(o != None, t._1.flatten, t._2) } |||
                t ^^ { case t => Transaction(false, t._1.flatten, t._2) }

    rep(_requires ~> ExprP <~ ";") ~
    rep(VarDeclP <~ ";") ~
    repsep(trans, _yield ~ ";") ^^ { case r ~ v ~ t => ExplicitMethodSpec(r, v, t) }
  }

  def ensureNonEmptySpec(spec: ExplicitMethodSpec) : ExplicitMethodSpec = {
    if (spec.transactions.isEmpty) {
      pos(ExplicitMethodSpec(spec.requires, Nil, List(pos(Transaction(false, Nil, List(pos(ConstExpr(   pos(BoolConst(true),spec.pos)), spec.pos))), spec.pos))), spec.pos)
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
          case r: RefType => Some(pos(ConstExpr(pos(NullConst(), stmt.pos)), stmt.pos))
          case x: CollectionType => Some(pos(ConstExpr(pos(EmptyCollectionConst(x), stmt.pos)), stmt.pos))
          case _ : TypeVar => Errors.fail("parser", "Bad return Type")
        }
        val fullSpec = if (t != VoidType()) {
          pos(ExplicitMethodSpec(spec.requires, spec.vars :+ pos(VarDecl(pos(t, spec.pos), "$result"), spec.pos), spec.transactions), spec.pos)
        } else {
          spec
        }

        MethodDecl(a.isDefined, t, i, params, (fullSpec), pos(Block(stmt.label, stmt.body :+ pos(Return(r, true), stmt.pos)), stmt))
      }
    }
  }

  def ConstructorDeclP: Parser[ConstructorDecl] = positioned {
    MethodSpecP ~ opt(_public) ~ Ident ~ "(" ~ repsep(VarDeclP, ",") ~ ")" ~ BlockP ^^ {
      case spec ~ a ~ name ~ _ ~ params ~ _ ~ stmt => {
        ConstructorDecl(a.isDefined, name, params, (spec), pos(Block(stmt.label, stmt.body :+ pos(Return(None, true), stmt.pos)), stmt))
      }
    }
  }

  def BaseTypeP: Parser[Type] = positioned {
    val voidType = _void ^^ { _ => VoidType() }
    val intType = _int ^^ { _ => IntType() }
    val boolType = _bool ^^ { _ => BoolType() }
    val tidType = _Tid ^^ { _ => TidType() }
    val classType = ClassTypeP ^^ { x => x }
    val typeVarType = TypeVarP
    val collectionType = (_set | _seq | _multiset | _map | _pair) ~ opt("<" ~> rep1sep(TypeP, ",") <~ ">") ^^ {
      case a ~ args => {
        val typeArgs = args.getOrElse(Nil)
        CollectionType(a, typeArgs)
      }
    }
    voidType | intType | boolType | classType | tidType | collectionType | typeVarType
  }

  def TypeP: Parser[Type] = positioned {
    BaseTypeP ||| ArrayTypeP
  }

  def VarDeclP: Parser[VarDecl] = positioned {
    TypeP ~ Ident ^^ { case t ~ i => VarDecl(t, i) }
  }

  def CloseBraceP: Parser[Positional] = positioned {
    "}" ^^ { _ => new Positional() {} }
  }

  def OpenBraceP: Parser[Positional] = positioned {
    "{" ^^ { _ => new Positional() {} }
  }

  def BlockP: Parser[Block] = positioned {
    opt(Ident ~ ":" ^^ { case i ~ _ => i }) ~ "{" ~ StmtsP ~ "}" ^^ { case n ~ _ ~ i ~ _ => Block(n, i) }
  }

  def StmtsP: Parser[List[Stmt]] = {
    rep(StmtHelper) ^^ { case x => x }
  }

  def InvariantP: Parser[Expr] = positioned {
    _invariant ~> ExprP
  }

  def VarDeclStmtP: Parser[VarDeclStmt] = positioned {
    VarDeclP ~ opt("=" ~> ExprP)  ^^ { case x ~ e => VarDeclStmt(x, e) }
  }

  def StmtHelper: Parser[Stmt] = {
    val AssignP: Parser[Assign] = positioned {
      LocationP ~ "=" ~ ExprP  ^^ { case x ~ _ ~ e => Assign(x, e) }
    }

    val LocalAssignP : Parser[Stmt] = positioned {
      AssignP ~ "," ~ rep1sep(AssignP, ",") ^^ { case a ~ _ ~ assigns => LocalAssign(a::assigns) }
    }

    // BlockP
    val ExprStmtP: Parser[Stmt] = positioned {
      ExprP ^^ { case x => ExprStmt(x) }
    }
    val IfP: Parser[Stmt] = positioned {
      _if ~ "(" ~ ExprP ~ ")" ~ BlockP ~ opt(_else ~> BlockP) ^^ {
        case _ ~ _ ~ e ~ _ ~ t ~ Some(f) => If(e, t, f)
        case _ ~ _ ~ e ~ _ ~ t ~ None => If(e, t, pos(Block(None, List[Stmt]()), e.pos))}
    }
//    val IfNoElseP: Parser[Stmt] = positioned {
//      _if ~ "(" ~ ExprP ~ ")" ~ BlockP ^^ { case _ ~ _ ~ e ~ _ ~ t => If(e, t, pos(Block(None, List[Stmt]()), e)) }
//    }
    val SyncP: Parser[Stmt] = positioned {
      _synchronized ~ "(" ~ LocationP ~ ")" ~ "{" ~ StmtsP ~ CloseBraceP ^^ { case _ ~ _ ~ i ~ _ ~ _ ~ s ~ cp => SyncBlock(i, pos(Block(None, s), if (s.isEmpty) i.pos else s.head.pos), cp.pos) }
    }
    val SyncStmtP: Parser[Stmt] = positioned {
      val SyncOpP : Parser[SyncOp] = positioned {
        _acquire ^^ { case _ => Acquire() } |
          _release ^^ { case _ => Release() } |
          _wait ^^ { case _ => Wait() } |
          _notify ^^ { case _ => Notify() }
      }
      SyncOpP ~ "(" ~ LocationP ~ ")" ^^ { case op ~ _ ~ i ~ _  => SyncStmt(op, i) }
    }

    val InvsAndDecs : Parser[(List[Expr], List[Expr])] = {
      val one =
        _invariant ~> ExprP <~ ";" ^^ { case x => (List[Expr](x), Nil) } |
        _decreases ~> ExprP <~ ";" ^^ { case x => (Nil, List[Expr](x)) }
      rep(one) ^^ { case x => {
        val (f,s) = x.unzip
        (f.flatten, s.flatten)
      }}
    }

    val WhileP: Parser[Stmt] = positioned {
      _while ~ "(" ~ ExprP ~ ")" ~ InvsAndDecs ~ BlockP ^^ { case _ ~ _ ~ e ~ _ ~ invsAndDecs ~ t => While(e, t, invsAndDecs._1, invsAndDecs._2) }
    }

    val BreakP: Parser[Stmt] = positioned {
      _break ^^ { _ => Break(None) }
    }

    val CommitP: Parser[Stmt] = positioned {
      _commit ^^ { case _ => Commit() }
    }

    val ReturnP: Parser[Stmt] = positioned {
      _return ~ opt(ExprP) ^^ { case _ ~ x  => Return(x, false) }
    }
    val AssumeP: Parser[Stmt] = positioned {
      _assume ~ ExprP ^^ { case _ ~ x  => Assume(x) }
    }
    val AssertP: Parser[Stmt] = positioned {
      _assert ~ ExprP  ^^ { case _ ~ x  => Assert(x) }
    }
    val InvariantP: Parser[Stmt] = positioned {
      _invariant ~ ExprP ^^ { case _ ~ x  => Invariant(x) }
    }
    val YieldP: Parser[Stmt] = positioned {
      _yield ~ repsep(_ensures ~> ExprP, ";")  ^^ { case _ ~ ensures => Yield(ensures) }
    }

    val BoogieP: Parser[Stmt] = positioned {
      _boogie ^^ { x => BoogieCode(x.substring(2)) }
    }

    val annotationP : Parser[Stmt] = positioned {
      _nocheck ~> StmtHelper ^^ { case s => NoReductionCheck(s) }
    }

    val SimpleStmt = VarDeclStmtP ||| AssignP ||| LocalAssignP ||| ExprStmtP ||| SyncStmtP ||| AssumeP ||| AssertP ||| YieldP ||| ReturnP ||| BreakP ||| SyncStmtP ||| InvariantP ||| CommitP
    val SimpleStmtSemi = SimpleStmt <~ ";"

    val ForP: Parser[Stmt] = positioned {
      _for ~ "(" ~ SimpleStmtSemi ~ ExprP ~ ";" ~ SimpleStmt ~ ")" ~ InvsAndDecs ~ BlockP ^^ { case _ ~ _ ~ init ~ e ~ _ ~ incr ~ _ ~ invsAndDecs ~ t => {
        Block(None, List(init, pos(While(e, pos(Block(t.label, t.body :+ incr), t.pos), invsAndDecs._1, invsAndDecs._2), e.pos)))
      }
    }}

    SimpleStmtSemi | IfP | SyncP | WhileP | BoogieP | ForP | annotationP
  }

//  def locationOrOldLocation : Parser[Location] = positioned {
//    LocationP | (_old ~ "(" ~ locationOrOldLocation ~ ")" ^^ { case _ ~ _ ~ l ~ _ => Old(l) })
//  }


  val triggersP : Parser[List[List[Expr]]] = {
    rep("{" ~> repsep(ExprP, ",") <~ "}")
  }


  val ExprP: Parser[Expr] = {

    val LocationExprP : Parser[Expr] = {
      val v: Parser[Location] = VarAccessP ~ rep(
        "." ~ Ident <~ not("(") ^^ { case _ ~ i => (x: Location) => pos(FieldAccess(x, i).asInstanceOf[Location], x) }
          | "#" ~ Ident <~ not("(") ^^ { case _ ~ i => (x: Location) => pos(FieldAccess(x, i, false).asInstanceOf[Location], x) }
          | "[" ~ ExprP ~ "]" ^^ { case _ ~ i ~ _ => (x: Location) => pos(ArrayAccess(x, i).asInstanceOf[Location],x) }
      ) ^^ { case x ~ es => es.foldLeft(x.asInstanceOf[Location])((l, f) => f(l)) }
      val tail : Parser[Expr => Expr] = {
        "." ~> _length ^^ { case _ => Length(_) } |||
          "." ~> _lock ^^ { case _ => Lock(_) } |||
          "." ~> Ident ~ "(" ~ repsep(ExprP, ",") ~ ")" ~ repsep(InvariantP, ";") ^^ { case  name ~ _ ~ exprs ~ _ ~ invs => Invoke(_, name, exprs, invs)} |||
         "[" ~ ExprP ~ ".." ~ ExprP ~ "]" ^^ { case _ ~ s ~ _ ~ l ~ _ => (x:Expr) => pos(BuiltInFunctionCall("SeqSub", None, List(x,s,l)), x)}
      }
      v ~ opt(tail) ^^
        { case l ~ Some(f) => f(l)
        case l ~ None => l }
    }

    val termP: Parser[Expr] =
      positioned {
        (_isLocal ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => IsLocal(x, pos(VarAccess("tid"), x)) }) |
          (_holds ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => Holds(x, pos(VarAccess("tid"), x)) }) |
          (_casok ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => NextCASSucceeds(x, pos(VarAccess("tid"), x)) }) |
          (_nextSpecStep ~> "(" ~> wholeNumber <~ ")") ^^ { case n => NextSpecStep(Integer.parseInt(n))} |
          (_isshared ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => IsShared(x) }) |
          (_isfresh ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ x ~ _ => IsFresh(x) }) |
          (_cas ~ "(" ~ LocationP ~ "," ~ Ident ~ "," ~ VarAccessP ~ "," ~ ExprP ~ ")" ^^ { case _ ~ _ ~ l ~ _ ~ i ~ _ ~ e ~ _ ~ v ~ _ => CAS(l, i, e, v) }) |
          Ident ~ opt("<" ~> rep1sep(TypeP, ",") <~ ">") ~ ("(" ~> repsep(ExprP, ",") <~ ")") ^^ { case name ~ typeArgs ~ args => BuiltInFunctionCall(name, typeArgs, args) } |
          PredefinedSyncDisciplinesP |
          (LocationExprP) |
          (_new ~ ClassTypeP ~ "(" ~ repsep(ExprP, ",") ~ ")" ~ repsep(InvariantP, ";") ^^ { case _ ~ name ~ _ ~ args ~ _ ~ invs => Alloc(name, args, invs) }) |
          (_new ~ ArrayTypeP ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ a ~ _ ~ size ~ _ => AAlloc(a, size) }) |
          (ConstP ^^ { x => ConstExpr(x) }) |
          (_old ~ "(" ~ ExprP ~ ")" ^^ { case _ ~ _ ~ l ~ _ => Old(l) }) |
          "(" ~ ExprP ~ ")" ^^ { case _ ~ x ~ _ => x /* UnaryExpr(x, Paren()) */ } |
          "[" ~> ExprP <~ "]" ^^ { case x => BuiltInFunctionCall("SeqUnit", None, List(x))} |
          "[" ~ "]" ^^ { case _~_ => BuiltInFunctionCall("SeqEmpty", None, List())}
      }


    val factor: Parser[Expr] = positioned {
      rep(UnaryOpP) ~ termP ^^ { case ops ~ e => ops.foldRight(e)({ case (op, e) => UnaryExpr(e, op) }) }
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

    val term = {
      val numeric = mkExpr(factor, List("*", "/", "%"))
      val op : Parser[(Expr,Expr) => Expr]=  {
        ("++" ^^ { case op => (a: Expr, b: Expr) => pos(BuiltInFunctionCall("SeqConcat", None, List(a,b)), a.pos)} )
      }
      chainl1(numeric, op)
    }
    val expr = mkExpr(term, List("+", "-"))
    val compExpr = {
      val e = mkExpr(expr, List("==", "!=", "<=", ">=", "<", ">"))
      e ^^ { case e => fixCompoundComparisions(e) }
    }
    val andExpr = mkExpr(compExpr, List("&&"))
    val orExpr = mkExpr(andExpr, List("||"))
    val impliesExpr = mkExpr(orExpr, List("==>"))

    val quantifierP : Parser[Quantifier] = {
      _forall ^^ { case _ => ForAll() } |
      _exists ^^ { case _ => Exists() }
    }

    val quantifiedExpr: Parser[Expr] = positioned {
      quantifierP ~ rep1sep(VarDeclP, ",") ~ "::" ~ triggersP ~ impliesExpr ^^ { case q ~ decls ~ _ ~ triggers ~ pred => Quantified(q, decls, pred, triggers) } |||
        impliesExpr
    }

    val slashExpr: Parser[Expr] = positioned {
      val hashP = positioned {
        "#" ^^ { case _ => new NoNode() }
      }
      val op : Parser[(Expr,Expr) => Expr]= {
        (hashP ^^ { case h => (x: Expr, y: Expr) => pos(Cond(pos(VarAccess("isRead"), x.pos), x, y), h.pos) })
      }

      val assertP = positioned {
        ">>>" ^^ { case _ => new NoNode() }
      }

      def assertCons(h : Position)(x: Expr, y: Expr) = pos(Cond(x, y, pos(ConstExpr(pos(MoverConst(E()), h)), h)), h)

      val aop : Parser[(Expr,Expr) => Expr]= {
        (assertP ^^ { case h => assertCons(h.pos) })
      }

      def build(es : List[Expr]):Expr = {
        es match {
          case a :: Nil => a
          case a :: as => {
            val rest = build(as)
            pos(Cond(a, rest, pos(ConstExpr(pos(MoverConst(E()), a.pos)), a.pos)), a.pos)
          }
        }
      }

      val term = chainl1(quantifiedExpr, op)
      rep1sep(term, ">>>") ^^ {
        case es => build(es)

      }
    }

    val condExpr: Parser[Expr] = positioned {
      quantifiedExpr ~ "?" ~ ExprP ~ ":" ~ ExprP ^^ { case p ~ _ ~ tt ~ _ ~ ff => Cond(p, tt, ff) } |
        slashExpr
    }

    val havoc: Parser[Expr] = positioned {
      "*" ^^ { case _ => Rand() }
    }

    positioned {
      condExpr | havoc
    }
  }

  def fixCompoundComparisions(expr: Expr) : Expr = {
    expr match {
      case BinaryExpr(b@BinaryExpr(lhs, rhs1, op1:OrderComparison), rhs2, op2:OrderComparison)                      => {
        BinaryExpr(fixCompoundComparisions(b),
          pos(BinaryExpr(fixCompoundComparisions(rhs1), fixCompoundComparisions(rhs2), op2), expr.pos),
          And())
      }
      case x => x
    }

  }

  def LocationP: Parser[Location] = positioned {
    val v: Parser[Location] = VarAccessP ~ rep(
      "." ~ Ident <~ not("(") ^^ { case _ ~ i => (x: Location) => pos(FieldAccess(x, i).asInstanceOf[Location], x) }
      | "#" ~ Ident <~ not("(") ^^ { case _ ~ i => (x: Location) => pos(FieldAccess(x, i, false).asInstanceOf[Location], x) }
      | "[" ~ ExprP ~ "]" ^^ { case _ ~ i ~ _ => (x: Location) => pos(ArrayAccess(x, i).asInstanceOf[Location],x) }
    ) ^^ { case x ~ es => es.foldLeft(x.asInstanceOf[Location])((l, f) => f(l)) }
    v
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
      wholeNumber ^^ { x => IntConst(x.toInt) }  |||
      _INT_MIN ^^ { _ => IntConst(Integer.MIN_VALUE/2)} |||
      _INT_MAX ^^ { _ => IntConst(Integer.MAX_VALUE/2)}
    }
    val nullValue = positioned {
    //  (ClassTypeP | ArrayTypeP) ~ "." ~ _null ^^ { case c ~ _ ~ _ => NullConst() }
      _null ^^ { case _ => NullConst() }
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

  def BuiltInFunctionDeclP: Parser[BuiltInFunctionDecl] = positioned {
      Ident ~
        opt("<" ~> rep1sep(typeVarIdent, ",") <~ ">") ~
        "(" ~
        repsep(TypeP, ",") ~ ")" ~ ":" ~ TypeP ~ ";" ^^ {
      case ident ~ typeVars ~ _ ~ paramTypes ~ _ ~ _ ~ returnType ~ _ => {
        BuiltInFunctionDecl(ident, typeVars.getOrElse(Nil), paramTypes, returnType)
      }
    }
  }

  private def parseIt[T](text: String, parser : Parser[T]): T = {
    this.parse(parser, text) match {
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
  def parseLib(s: String) = parseIt(s, rep(BuiltInFunctionDeclP))
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

  def spec(expr: String): MoverSpec = {
    new Parser().parseSpec(expr)
  }

  def lib(s : String) : List[BuiltInFunctionDecl] = {
    new Parser().parseLib(s)
  }

}
