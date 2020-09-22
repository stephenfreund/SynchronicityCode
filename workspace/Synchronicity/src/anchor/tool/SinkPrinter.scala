package anchor.tool

import acme.scala.Util
import anchor.boogie._
import anchor.sink._
import anchor.util.Errors._
import anchor.transforms.Rename

import scala.language.implicitConversions
import scala.util.parsing.input.NoPosition

class StmtAnnotation(val s: Stmt) extends Annotation {
  override def toString() = {
    s"(${s.getClass}:${s.pos})"
  }
}

class MethodAnnotation(val x: MethodDecl) extends Annotation {
  override def toString() = {
    s"(Method:${x.pos})"
  }
}

case class VerifyOptions  (val checkSpec: Boolean,
                           val checkPNP: Boolean,
                           val checkConsistency: Boolean,
                           val checkMethodSpec: Boolean,
                           val yeildsAreNoOps: Boolean,
                           val houdini: Boolean)

object VerifyOptions {
  val noVerify = VerifyOptions(false, false, false, false, false, false)
  val all = VerifyOptions(true, true, false, false, false, false)
  val methodSpecs = VerifyOptions(true, true, false, true, false, false)
  val consistency = VerifyOptions(true, true, true, false, false, false)
  val seqHoudini = VerifyOptions(false, false, false, false, true, true)
  def apply(s: String): VerifyOptions = s match {
    case "None"    => noVerify
    case "All"     => all
    case "MethodSpecs" => methodSpecs
    case "Sequential" => VerifyOptions(false, true, false, false, true, false)
    case "SyncSpec"   => VerifyOptions(true, false, false, false, false, false)
    case "PNP"    => VerifyOptions(false, true, false, false, false, false)
    case "Sanity"  => VerifyOptions(false, false, false, false, false, false)
    case "Houdini" => VerifyOptions(true, true, false, false, false, true)
    case "SeqHoudini" => seqHoudini
    case "Consistent" => consistency
    case _         => acme.util.Assert.fail("Bad verify option: " + s); noVerify
  }
}


object InvariantPrefix extends Enumeration {
  type InvariantPrefix = Value
  val requires, ensures, invariant, assert, assume = Value
}


class SinkPrinter(val config: Config, val program: Program, val options: VerifyOptions) {

  val methodSpecPrefix = "$spec$"
  val oldSuffix = "$old"

  val classes: List[ClassDecl] = program.classes

  var inReductionCheck = false
  var overrideCheckReduction = false

  private def reductionCheckOrElse[T](code: => T)(orElse: => T): T = {
    if (options.checkPNP && !overrideCheckReduction) {
      val tmp = inReductionCheck
      inReductionCheck = true
      val result = code
      inReductionCheck = tmp
      result
    } else {
      orElse
    }
  }

  private def noReductionCheck[T](code: => T): T = {
    val tmp = overrideCheckReduction
    overrideCheckReduction = true
    val result = code
    overrideCheckReduction = tmp
    result
  }

  private def reductionCheck(code: => Unit): Unit = {
    if (options.checkPNP && ! overrideCheckReduction) {
      val tmp = inReductionCheck
      inReductionCheck = true
      code
      inReductionCheck = tmp
    }
  }



  private def noReductionCheck(code: => Unit): Unit = {
    val tmp = overrideCheckReduction
    overrideCheckReduction = true
    code
    overrideCheckReduction = tmp
  }

  private def methodSpecCheck(code: => Unit): Unit = {
    if (options.checkMethodSpec) {
      code
    }
  }

  private def methodSpecCheckOrElse[T](code: => T)(orElse: => T): T = {
    if (options.checkMethodSpec) {
      code
    } else {
      orElse
    }
  }



  private def genNotNullBlockingOperation(buffer: AnnotatedBuffer, x: Stmt, v: VarAccess): Unit = {
    val VarAccess(l) = v
    val name = pp(v.decl.t)
    reductionCheck {
      buffer.emit("if (_pc == PreCommit) {")
      buffer.push()
    }
    buffer.emit(s"""assume ${v.name} != ${name}.null;""")
    reductionCheck {
      buffer.pop()
      buffer.emit("} else {")
      buffer.push()
      buffer.emit(s"""assert ${v.name} != ${name}.null;""", ReductionError(x, "Cannot have potential null deference in left-mover part."))
      buffer.pop()
      buffer.emit("}")
    }
  }

  private def genAssumeOrAssert(buffer: AnnotatedBuffer, s: String, error: Option[SinkError]): Unit = {

    reductionCheck {
      buffer.emit("if (_pc == PreCommit) {")
      buffer.push()
    }
    buffer.emit(s"""assume $s;""")
    reductionCheck {
      buffer.pop()
      buffer.emit("} else {")
      buffer.push()
      buffer.emit(s"""assert $s;""", error)
      buffer.pop()
      buffer.emit("}")
    }
  }

  // checkCooperative only
  private def assertInPreCommit(buffer: AnnotatedBuffer, x: Stmt): Unit = {
    buffer.emit("assert _pc == PreCommit;", ReductionError(x, "Cannot have blocking operation in left-mover part of transaction."))
  }


  /////////

  private def asParam(f: FieldDecl, suffix: String = ""): String = s"${f.name}${suffix} : ${pp(f.t)}"

  private def asArg(f: FieldDecl, suffix: String = ""): String = s"${f.name}${suffix}"

  private def gen(buffer: AnnotatedBuffer, c: ClassDecl): Unit = {
    val name = c.name
    val fields = c.fields
    val nullValue = s"${name}.null"

    def fs(f: FieldDecl => String) = {
      (fields map f).mkString
    }

    def params(suffix: String = "") = fs(f => s", ${asParam(f, suffix)}")

    def args(suffix: String = "") = fs(f => s", ${asArg(f, suffix)}")

    buffer.emit(
      s"""
      /*** Class Decl ${c.name} ***/

      type ${name};
      const unique ${name}.null: ${name};
      var ${name}._state: [${name}]State;

      """)

    for (f <- c.fields) {
      buffer.emit("/////")
      buffer.emit()
      buffer.emit(s"var ${name}.${f.name}: [${name}]${pp(f.t)};")
      buffer.emit()
      genEvalFunction(buffer, f, false)
      buffer.emit()
      genEvalFunction(buffer, f, true)
      buffer.emit()
      buffer.emit("/////")
      buffer.emit()
    }

    buffer.emit()
    buffer.emit("/////")
    buffer.emit()

    for (inv <- c.invariants) {
      genInvariantFunction(buffer, c, inv)
      buffer.emit()
    }

    buffer.emit("/////")
    buffer.emit()

    for (m <- c.methods if m.isPublic) {
      gen(buffer, m)
      buffer.emit()
    }
  }

  private def genSpecChecks(buffer: AnnotatedBuffer) = {
    val checks = new SpecChecks(buffer, this)

    var count = 0

    def unique() = {
      count += 1; count
    }

    val specs: List[HasSpec] = program.classes.flatMap(c => {
      c.fields.filter(!_.isInternal).filter(_.name != "_lock") ++ c.arrays
    })

    checks.check(specs)
  }

  private def localShare(buffer: AnnotatedBuffer, x: Stmt, obj: VarOrConst): Unit = {
    obj match {
      case ConstExpr(const) =>
      case y: VarAccess     => localShare(buffer, x, y)
    }
  }

  private def fieldsInSource(c : ClassDecl) = {
    c.fields.filter(!_.isInternal)
  }

  private def allFieldsInSource() = {
    program.classes.flatMap(fieldsInSource(_))
  }

  private def localShare(buffer: AnnotatedBuffer, x: Stmt, obj: VarAccess): Unit = {
    val t = obj.decl.t
    if (Type.isHeapReference(t)) {
      val name = pp(t)
      buffer.emit(s"if (isLocal(${name}._state[${pp(obj)}], tid)) {")
      buffer.push()
      buffer.emit(s"${name}._state[${pp(obj)}] := SHARED();")
      t match {
        case c@ClassType(ident)                     =>
          for (f <- fieldsInSource(c.decl)) {
            if (Type.isHeapReference(f.t)) {
              buffer.emit(s"assert isSharedAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[${pp(obj)}]]);",
                SharingError(x, s"${pp(obj)} became shared, but ${pp(obj)}.${f.name} may not be shared."))
            }
          }
        case t@ArrayType(prefix, ident, thisAccess) =>
          if (Type.isHeapReference(t.decl.elemType)) {
            buffer.emit(
              s"assert (forall _index : int :: 0 <= _index && _index < ${pp(t)}._length[${pp(obj)}] ==> isSharedAssignable(${pp(t.decl.elemType)}._state[${pp(t)}._elems[${pp(obj)}][_index]]));",
              SharingError(x, s"${pp(obj)} became shared, but some element may not be shared."))
          }

          // also share owner if we are sharring array....
          buffer.emit(s"if (isLocal(${t.decl.parent.name}._state[${name}._this[${pp(obj)}]], tid)) {")
          buffer.push()
          buffer.emit(s"${t.decl.parent.name}._state[${name}._this[${pp(obj)}]] := SHARED();")
          for (f <- fieldsInSource(t.decl.parent)) {
            if (Type.isHeapReference(f.t)) {
              buffer.emit(s"assert isSharedAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[${name}._this[${pp(obj)}]]]);",
                SharingError(x, s"${pp(obj)} became shared, but ${name}._this[${pp(obj)}].${f.name} may not be shared."))
            }
          }

          buffer.pop()
          buffer.emit(s"}")
        case _                                      =>
      }
      buffer.pop()
      buffer.emit("}")
      buffer.emit()
    }
  }


  private def localShareDeferredChecks(buffer: AnnotatedBuffer, x: Stmt, obj: VarOrConst): () => Unit  = {
      obj match {
        case ConstExpr(const) => () => { }
        case y: VarAccess     => localShareDeferredChecks(buffer, x, y)
      }
    }

    private def localShareDeferredChecks(buffer: AnnotatedBuffer, x: Stmt, obj: VarAccess): () => Unit = {
    val t = obj.decl.t
    if (Type.isHeapReference(t)) {
      val name = pp(t)
      buffer.emit(s"assert isLocal(${name}._state[${pp(obj)}], tid);", SharingError(x, "Can only have local designators in Local Write block."))
//      buffer.emit(s"${name}._state[${pp(obj)}] := SHARED();")
      t match {
        case c@ClassType(ident)                     => {
          () => {
            for (f <- fieldsInSource(c.decl)) {
              if (Type.isHeapReference(f.t)) {
                buffer.emit(s"assert isSharedAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[${pp(obj)}]]);",
                  SharingError(x, s"${pp(obj)} became shared, but ${pp(obj)}.${f.name} may not be shared."))
              }
            }
          }
        }
        case t@ArrayType(prefix, ident, thisAccess) => {
  //        buffer.emit(s"${t.decl.parent.name}._state[${name}._this[${pp(obj)}]] := SHARED();")

          () => {
            if (Type.isHeapReference(t.decl.elemType)) {
              buffer.emit(
                s"assert (forall _index : int :: 0 <= _index && _index < ${pp(t)}._length[${pp(obj)}] ==> isSharedAssignable(${pp(t.decl.elemType)}._state[${pp(t)}._elems[${pp(obj)}][_index]]));",
                SharingError(x, s"${pp(obj)} became shared, but some element may not be shared."))
            }

            // also share owner if we are sharring array....
            buffer.emit(s"${t.decl.parent.name}._state[${name}._this[${pp(obj)}]] := SHARED();")
            for (f <- fieldsInSource(t.decl.parent)) {
              if (Type.isHeapReference(f.t)) {
                buffer.emit(s"assert isSharedAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[${name}._this[${pp(obj)}]]]);",
                  SharingError(x, s"${pp(obj)} became shared, but ${name}._this[${pp(obj)}].${f.name} may not be shared."))
              }
            }
          }
        }
        case _                                      => { () => {} }
      }
    } else {
      () => { }
    }
  }

  private def shareNoChecks(buffer: AnnotatedBuffer, x: Stmt, obj: VarOrConst): Unit  = {
    obj match {
      case ConstExpr(const) => () => { }
      case y: VarAccess     => shareNoChecks(buffer, x, y)
    }
  }

  private def shareNoChecks(buffer: AnnotatedBuffer, x: Stmt, obj: VarAccess): Unit = {
    val t = obj.decl.t
    if (Type.isHeapReference(t)) {
      val name = pp(t)
      buffer.emit(s"${name}._state[${pp(obj)}] := SHARED();")
      t match {
        case t@ArrayType(prefix, ident, thisAccess) => {
          buffer.emit(s"${t.decl.parent.name}._state[${name}._this[${pp(obj)}]] := SHARED();")
        }
        case _                                      => {  }
      }
    }
  }

  private def genGlobals(buffer: AnnotatedBuffer) = {
    for (g <- program.globals) {
      if (g.t == BoolType()) {
        buffer.emit(s"const {:existential true} ${g.name} : bool;")
      } else {
        buffer.emit(s"const unique ${g.name} : ${pp(g.t)};")
      }
    }
  }

  private def genBackGround(buffer: AnnotatedBuffer) = {
    buffer.emit(
      s"""
    /*
     * Tid
     */
    type Tid = int;  // make int so you can iterate over Tids
    const unique Tid.null: Tid;
    axiom Tid.null == -1;

    function {:inline} ValidTid(tid : Tid): bool {
    @ tid != Tid.null && tid >= 0 ${if (config.maxTid != -1) s" && tid <= ${config.maxTid}" else ""}
    }

    type{:datatype} State;
    function{:constructor} NULL(): State;
    function{:constructor} FRESH(): State;
    function{:constructor} LOCAL(t: Tid): State;
    function{:constructor} SHARED(): State;

    function {:inline} isNull(state: State) : bool {
    @ state == NULL()
    }

    function {:inline} isFresh(state: State) : bool {
    @ state == FRESH()
    }

    function {:inline} isShared(state: State) : bool {
    @ state == SHARED()
    }

    function {:inline} isLocal(state: State, t: Tid) : bool {
    @ state == LOCAL(t)
    }

    function {:inline} isLocalAssignable(state: State, t: Tid) : bool {
    @ state == LOCAL(t) || state == SHARED() || state == NULL()
    }

    function {:inline} isSharedAssignable(state: State) : bool {
    @ state == SHARED() || state == NULL()
    }

    function {:inline} isAccessible(state: State, t: Tid) : bool {
    @ state == LOCAL(t) || state == SHARED()
    }

    function {:inline} isAllocated(state: State) : bool {
    @ !isFresh(state) && !isNull(state)
    }

    ${if (this.config.modAxioms) """
         axiom (forall x,y,z : int :: { (x mod y) mod z } (y > 0 && z > 0 && y mod z == 0) ==> (x mod z == (x mod y) mod z));
         axiom (forall x,y : int :: { x mod y } (0 <= x && x < y) ==> (x == x mod y));
         axiom (forall x,y,z : int :: (z == x + x && x mod y == 0 ==> z mod y == 0));
    """ else """
        function MOD(x:int, y:int): int;
        """}

    /*
     * For triggers
     */
    function {:inline false} _trigger(i: int): bool {  true  }

    """)
  }

  private def genCooperativeBackground(buffer: AnnotatedBuffer) =
    buffer.emit(
      """

    type Phase;
    const unique PreCommit : Phase;
    const unique PostCommit : Phase;
    const unique PhaseError : Phase;

    function {:inline} transition(p: Phase, m: Mover): Phase {
    @  if (m == _B) then
    @@    p
    @  else if (m == _R) then
    @@    if (p == PreCommit) then
    @@@      PreCommit
    @@    else
    @@@      PhaseError
    @  else if (m == _L) then
    @@    if (p == PostCommit) then
    @@@      PostCommit
    @@    else if (p == PreCommit) then
    @@@      PostCommit
    @@    else
    @@@      PhaseError
    @  else if (m == _N) then
    @@    if (p == PreCommit) then
    @@@      PostCommit
    @@    else
    @@@      PhaseError
    @  else
    @@   PhaseError // m == E or m == I
    }


    type Mover;
    const unique _B : Mover;
    const unique _R : Mover;
    const unique _L : Mover;
    const unique _N : Mover;
    const unique _E : Mover;

    axiom (forall m : Mover :: m == _B || m == _R || m == _L || m == _N || m == _E);

    function {:inline} leq(m1: Mover, m2: Mover) : bool {
    @  if (m1 == _B) then
    @@    true
    @  else if (m1 == _R) then
    @@    m2 == _R || m2 == _N || m2 == _E
    @  else if (m1 == _L) then
    @@    m2 == _L || m2 == _N || m2 == _E
    @  else if (m1 == _N) then
    @@    m2 == _N || m2 == _E
    @  else if (m1 == _E) then
    @@    m2 == _E
    @  else
    @@    false // should never happen...
    }

    function {:inline} lt(m1: Mover, m2: Mover) : bool { m1 != m2 && leq(m1, m2) }

    function {:inline} isError(m : Mover) : bool {
    @ m == _E
    }

    function {:inline} eqOrError(m : Mover, n : Mover) : bool {
    @ m == n || m == _E
    }

    type{:datatype} MoverPath;
    function{:constructor} moverPath(m:Mover, p:int):MoverPath;

    """)

  private def gen(buffer: AnnotatedBuffer, c: Spec): Unit = {
    def choiceAsInt(x: List[Int]) = {
      x.foldLeft(0)(_ * 2 + _)
    }

    def gen(buffer: AnnotatedBuffer, c: Expr, choice: List[Int]): Unit = {
      c match {
        case ConstExpr(m)  => buffer.emit(s"moverPath(${pp(m)}, ${choiceAsInt(choice)})")
        case Cond(p, t, f) =>
          buffer.emit(s"if (${pp(p)}) then")
          buffer.push()
          gen(buffer, t, 1 :: choice)
          buffer.pop()
          buffer.emit("else")
          buffer.push()
          gen(buffer, f, 0 :: choice)
          buffer.pop()
        case UnaryExpr(expr, Paren())             => s"(${gen(buffer, expr, choice)})"
        case _ => assert(false);
      }
    }

    gen(buffer, c.conditionalMover, Nil)
  }

  // FIXME: Nasty code...
  private def genEvalFunction(buffer: AnnotatedBuffer, f: FieldDecl, isWrite: Boolean): Unit = {
    var params = List(s"tid: Tid", s"this : ${f.parent.name}")
    if (isWrite) {
      params = params :+ s"newValue: ${pp(f.t)}"
    }

    params ++= genHeapParameterList()


    buffer.emit(s"""function {:inline} ${if (isWrite) "Write" else "Read"}Eval.${f.parent.name}.${f.name}(${params.mkString(",")}) returns (MoverPath) {""")
    buffer.push()

    val cm = f.spec
    buffer.emit(s"(var isRead := ${!isWrite}; ")
    if (!isWrite) {
      buffer.emit(s"(var newValue := ${pp(defaultValue(f.t))}; ")
    }
    gen(buffer, cm)
    buffer.emit(")")
    if (!isWrite) {
      buffer.emit(s")")
    }

    buffer.pop()
    buffer.emit("}")
  }

  private def genEvalFunction(buffer: AnnotatedBuffer, array: ArrayDecl, isWrite: Boolean): Unit = {
    val name = s"Array.${array.parent.name}.${array.name}"

    var params = List("tid: Tid", s"this : ${array.parent.name}", s"athis : ${name}", s"${array.elemName} : int")
    if (isWrite) {
      params = params :+ s"newValue: ${pp(array.elemType)}"
    }
    params ++= genHeapParameterList()

    val cm = array.spec
    buffer.emit(s"// ${name}: ${cm}")
    buffer.emit()
    buffer.emit(s"""function {:inline} ${if (isWrite) "Write" else "Read"}Eval.Array.${array.parent.name}.${array.name}(${params.mkString(",")}) returns (MoverPath) {""")
    buffer.push()

    buffer.emit(s"(var isRead := ${!isWrite}; ")
    gen(buffer, cm)
    buffer.emit(")")

    buffer.pop()
    buffer.emit("}")
  }

  // type/name pairs
  def allStateVariables(): List[(String, String)] = {
    var list = Array[String]()
    for (c <- classes) {
      list = list :+ s"${c.name}._state: [${c.name}]State"
      for (f <- c.fields) {
        list = list :+ (s"${c.name}.${f.name} : [${c.name}]${pp(f.t)}")
      }
      for (a <- c.arrays) {
        val name = s"Array.${a.parent.name}.${a.name}"
        list = list :+ (s"${name}._state : [${name}]State")
        list = list :+ (s"${name}._elems : [${name}]([int]${pp(a.elemType)})")
        list = list :+ (s"${name}._length : [${name}]int")
      }
    }
    list.map(x => {
      val y = x.split(":");
      (y(1).trim, y(0).trim)
    }).toList
  }

  def genHeapLocalList(prefix : String = "", suffix : String = "") = {
    allStateVariables().map(x => ("var " + prefix + x._2 + suffix + ": " + x._1 + ";"))
  }

  def genHeapParameterList(prefix : String = "", suffix : String = "") = {
    allStateVariables().map(x => (prefix + x._2 + suffix + ": " + x._1))
  }

  def genHeapArgList(prefix : String = "", suffix : String = "") = {
    allStateVariables().map(x => (prefix + x._2 + suffix))
  }

  private def genInvariantFunction(buffer: AnnotatedBuffer, c: ClassDecl, inv: ClassInvariant): Unit = {
    var params = List(s"tid: Tid", s"this : ${c.name}")
    params ++= genHeapParameterList()


    buffer.emit(s"""function {:inline} Invariant.${c.name}.${inv.id}(${params.mkString(",")}) returns (bool) {""")
    buffer.push()

    buffer.emit(pp(inv.pred))

    buffer.pop()
    buffer.emit("}")
  }

  // returns (guard, call)
  def genInvariantCall(c: ClassDecl, inv: ClassInvariant, tid: String, obj: String, heapPrefix : String = "", heapSuffix: String = ""): (String, String) = {
    var params = List(s"$tid: Tid", s"$obj : ${c.name}")
    params ++= genHeapArgList(heapPrefix, heapSuffix)
    val guard = inv.pred match {
      case BinaryExpr(VarAccess(name), _, Implies()) => name + " ==> "
      case _                                         => ""
    }
    (guard, s"Invariant.${c.name}.${inv.id}(${params.mkString(",")})")
  }

  def preserveState(scope: SymTab, suffix: String, heapVarSuffix: String = "", ctxt: SinkStmtContext = new SinkStmtContext()): (Set[VarDecl], String) = {
    this.preserveState(scope.allLocals, suffix, heapVarSuffix, ctxt)
  }

  def preserveState(localVars: List[VarDecl], suffix: String, heapVarSuffix: String, ctxt: SinkStmtContext): (Set[VarDecl], String) = {
    val allHeapVars = allStateVariables()

    val heapDecls = allHeapVars.map(x => VarDecl(BoogieType(x._1), x._2 + suffix)).toSet

    val localDecls = localVars.map(x => VarDecl(x.t, x.name + suffix)).toSet +
      VarDecl(BoogieType("Phase"), s"_pc${suffix}") +
      VarDecl(IntType(), s"$$recorded.state${suffix}")

    val (methodSpecDecls, methodSpecVars) = declsAndVarsForMethodSpec(ctxt, suffix)

    val extras: Set[String] =
      (if (inReductionCheck) {
        Set[String]("_pc")
      } else {
        Set[String]()
      })

    val names = allHeapVars.map(x => (x._2 + suffix, x._2 + heapVarSuffix)) ++
      (localVars).map(x => (x.name + suffix, x.name)) ++
      (extras ++ methodSpecVars).map(x => (x + suffix, x))

    val code =
      s"""assume ${names.map(x => x._1 + " == " + x._2).mkString(" && ")};
         | assume $$recorded.state${suffix} == 1;""".stripMargin

    (heapDecls ++ localDecls ++ methodSpecDecls, code)
    //(Set.empty, "")
  }

  private def genEvalCall(name: String, initialParams: Array[String], isOld: Boolean): String = {
    val params = initialParams ++ genHeapArgList().map(a => if (isOld) s"old($a)" else a)
    s"${name}(${params.mkString(",")})"
  }

  def genReadEvalCall(f: FieldDecl, thisName: String, tidName: String, isOld: Boolean): String = {
    genEvalCall(s"ReadEval.${f.parent.name}.${f.name}", Array(s"${tidName}: Tid", s"${thisName}: ${f.parent.name}"), isOld)
  }


   def genWriteEvalCall(f: FieldDecl, thisName: String, tidName: String, newValueName: String, isOld: Boolean): String = {
    genEvalCall(s"WriteEval.${f.parent.name}.${f.name}", Array(s"${tidName}: Tid", s"${thisName}: ${f.parent.name}", s"${newValueName}: ${pp(f.t)}"), isOld)
  }

   def genReadEvalCall(a: ArrayDecl, thisName: String, athisName: String, tidName: String, indexName: String, isOld: Boolean): String = {
    genEvalCall(s"ReadEval.Array.${a.parent.name}.${a.name}", Array(s"${tidName}: Tid", s"${thisName}: ${a.parent.name}", s"${athisName}: Array.${a.parent.name}.${a.name}", s"${indexName}: int"), isOld)
  }

   def genWriteEvalCall(a: ArrayDecl, thisName: String, athisName: String, tidName: String, indexName: String, newValueName: String, isOld: Boolean): String = {
    genEvalCall(s"WriteEval.Array.${a.parent.name}.${a.name}", Array(s"${tidName}: Tid", s"${thisName}: ${a.parent.name}", s"${athisName}: Array.${a.parent.name}.${a.name}", s"${indexName}: int", s"${newValueName}: ${pp(a.elemType)}"), isOld)
  }



  private def genStateInvariantFunction(buffer: AnnotatedBuffer): Unit = {

    buffer.emit(s"function {:inline} StateInvariant(${genHeapParameterList().mkString(",")}) returns (bool) {")

    val clauses = new AnnotatedBuffer(" &&\n")
    clauses.emit("true")

    // heap structure

    def statePropertyInvariants(name : String) = {
      clauses.emit(s"""(forall _i: ${name}  :: _i == ${name}.null <==> isNull(${name}._state[_i]))""")
    }

    for (c <- classes) {
      statePropertyInvariants(c.name)
      for (a <- c.arrays) {
        statePropertyInvariants(s"Array.${a.parent.name}.${a.name}")
      }
    }

    for (c <- classes) {
      for (a <- c.arrays) {
        val name = s"Array.${a.parent.name}.${a.name}"
        clauses.emit(s"""(forall _t: Tid, _i: ${name}  :: ValidTid(_t) && isAccessible(${name}._state[_i], _t) ==> isAccessible(${a.parent.name}._state[${name}._this[_i]], _t))""")
      }
    }

    for (g <- program.globals if Type.isHeapReference(g.t)) {
      clauses.emit(s"isShared(${pp(g.t)}._state[${g.name}])")
    }

    for (c <- classes) {
      for (f <- fieldsInSource(c)) {
        if (Type.isHeapReference(f.t)) {
          val triggers = s"{ ${pp(f.t)}._state[${f.parent.name}.${f.name}[_i]] }"
          val ltriggers = ""
          clauses.emit(s"(forall _i: ${f.parent.name} :: ${triggers} (isShared(${f.parent.name}._state[_i]) ==> isSharedAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[_i]])))")
          clauses.emit(s"(forall _i: ${f.parent.name} :: ${triggers} (forall _t: Tid :: (ValidTid(_t) && isLocal(${f.parent.name}._state[_i],_t) ==> isLocalAssignable(${pp(f.t)}._state[${f.parent.name}.${f.name}[_i]], _t))))")
//          (forall _i: Cow :: { Cow._state[Cow.y[_i]] } (forall _t: Tid :: (isLocal(Cow._state[_i],_t) ==> isLocalAssignable(Cow._state[Cow.y[_i]], _t)))) &&


        }
        if (Type.isArray(f.t)) {
          val triggers = s"{ ${f.parent.name}.${f.name}[_i] }"
          clauses.emit(s"(forall _i: ${f.parent.name} :: ${triggers} ${pp(f.t)}._this[${f.parent.name}.${f.name}[_i]] == _i)")
        }
      }
      for (a <- c.arrays if Type.isHeapReference(a.elemType)) {
        val name = s"Array.${a.parent.name}.${a.name}"
        val triggers = s"{ $name._elems[_i][_index] }"
        val ltriggers = ""
        clauses.emit(s"""(forall _i: $name, _index: int :: ${triggers} (isShared($name._state[_i]) && 0 <= _index && _index < $name._length[_i]) ==> isSharedAssignable(${pp(a.elemType)}._state[$name._elems[_i][_index]]))""")
        clauses.emit(s"""(forall _i: $name, _index: int :: ${triggers} (forall _t: Tid :: (ValidTid(_t) && isLocal($name._state[_i],_t) && 0 <= _index && _index < $name._length[_i]) ==> isLocalAssignable(${pp(a.elemType)}._state[$name._elems[_i][_index]], _t)))""")
      }
    }

    clauses.emit("_trigger(0)")
    clauses.emit("_trigger(1)")
    clauses.emit("_trigger(2)")
    clauses.emit("_trigger(3)")

    buffer.push()
    buffer.emit("\n", clauses, "")
    buffer.pop()
    buffer.emit("}")
  }

  def genStateInvariantCall(prefix: String = "", suffix: String = ""): String = {
    s"StateInvariant(${genHeapArgList(prefix, suffix).mkString(", ")})"
  }

  /////////


  private def genArray(buffer: AnnotatedBuffer, array: ArrayDecl) = {
    val elemType = array.elemType

    val elem = pp(elemType)
    val name = s"Array.${array.parent.name}.${array.name}"

    val contents =
      s"""
         |    /*** Array ${name} ***/
         |
         |    type ${name};
         |    const unique ${name}.null: ${name};
         |    var ${name}._state: [${name}]State;
         |
         |    const ${name}._this : [$name]${array.parent.name};
         |    const ${name}._length : [$name]int;
         |    var ${name}._elems  : [$name]([int]$elem);
         |
         |    axiom (forall $$this : ${name} :: ${name}._length[$$this] >= 0);
         |
       """.stripMargin

    buffer.emit(contents)

    buffer.emit()
    buffer.emit()
    buffer.emit()
    buffer.emit()
  }

  ////////

  private def gen(buffer: AnnotatedBuffer): Unit = {
    buffer.emit("//// Background")
    buffer.emit()
    buffer.push()

    genBackGround(buffer)

    genCooperativeBackground(buffer)

    buffer.pop()


//    buffer.emit("//// Collections")
//    buffer.emit()
//    buffer.push()
//    buffer.emit(Collections.boogieText)
//    buffer.pop()


    buffer.emit()

    buffer.emit()
    buffer.emit("//// axioms")
    buffer.emit()
    for (x <- program.axioms) {
      buffer.emit(s"axiom ${pp(x)};")

    }

    buffer.emit()
    buffer.emit("//// classes")
    buffer.emit()

    for (c <- classes) {
      gen(buffer, c)
      for (array <- c.arrays) {
        genArray(buffer, array)
        buffer.emit()
        genEvalFunction(buffer, array, false)
        buffer.emit()
        genEvalFunction(buffer, array, true)
        buffer.emit()
      }
      buffer.emit()
      buffer.emit()
    }

    buffer.emit()
    buffer.emit("//// Globals")
    buffer.emit()
    buffer.push()
    genGlobals(buffer)
    buffer.pop()

    buffer.emit()
    buffer.emit("//// State Invariant")
    buffer.emit()
    buffer.push()
    genStateInvariantFunction(buffer)
    buffer.pop()

    if (options.checkSpec) {
      buffer.emit("//// Spec Checks")
      buffer.emit()
      buffer.push()
      genSpecChecks(buffer)
      buffer.pop()
    }

    new SinkYieldPrinter(buffer, config,this, program).gen()
  }

  def genBoogie(preamble: String = ""): AnnotatedText = {
    val buffer = new AnnotatedBuffer()

    buffer.emitPreservingIndentation(preamble)

    gen(buffer)

    for (t <- GatherCollections.apply(program)) {
      gen(buffer, t)
    }

    buffer.contents
  }

  private def gen(buffer: AnnotatedBuffer, t: CollectionType) = {
    buffer.emit(s"/// ${pp(t)}: ")
    val text = t.decl.gen(this, t.typeArgs)
    buffer.emit(text)
  }

  private def genHeapInvariants(x: MethodDecl, prefix: String): AnnotatedBuffer = {
    val buffer = new AnnotatedBuffer();
    buffer.emit(s"${prefix} ValidTid(tid);", InternalError(x, s"Bad tid"))

    // parameter must be allocated and shared
    def parameterRequirements(p: VarDecl) = {
      buffer.emit(s"${prefix} isSharedAssignable(${pp(p.t)}._state[${p.name}]);", InternalError(x, s"Parameter $p is not global"))
    }

    for (p <- x.params) {
      if (Type.isObject(p.t) || Type.isArray(p.t)) {
        parameterRequirements(p)
      }
    }

    if (x.name != "init") {
      // this is not null
      buffer.emit(s"${prefix} isShared(${x.parent.name}._state[this]);", InternalError(x, "this is not global"))
    } else {
      buffer.emit(s"${prefix} isLocal(${x.parent.name}._state[this], tid);")
    }
    buffer
  }

  import InvariantPrefix.InvariantPrefix

  def genGlobalInvariants(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, prefix : InvariantPrefix, node : ASTNode, stmtForFilter : Option[Stmt] = None) = {
    val method = ctxt.method

    val filter = new Modifies.InvariantFilter(stmtForFilter)

    for (c <- program.classes;
         i <- c.invariants) { // if (filter.killsObjectInvariant(i, c))) {
      val error = InvariantError(node, i.pred, s"Object invariant may not hold.")
      // NOTE: We cannot compare decl ptrs here, because after inlining the decls for invokes point to the original program.
      // we also include the self object for public constructors
      val vars = ctxt.inlined.filter(i => i.decl.name == "init" && i.decl.parent.name == c.name).map(_.ref.name) ++
        (ctxt.method match {
          // note: take "this" out of ignore set if we are at a return, which means we are exiting a constructor.
          case Some(m: MethodDecl) if (m.name == "init" && m.parent == c && !node.isInstanceOf[Return]) => List("this")
          case _                                                                                        => List.empty
        })
      val avoidThis = if (prefix != InvariantPrefix.ensures && vars.size > 0) {
        vars.map(s => s"_this != ${s}").mkString(" && ")
      } else {
        "true"
      }
      val text = genGlobalInv(c, i, "_this", avoidThis)
      buffer.emit(s"${prefix} ${text};", error)
    }
  }

  def genGlobalInv(c : ClassDecl, i : ClassInvariant, thisName : String, avoidThis : String, heapPrefix : String = "", heapSuffix: String = "") = {
    val newThis = VarDecl(ClassType(c), thisName)
    val renamedPred = Rename(i.pred, i.pred.scope.resolveVar("this").get, newThis)
    val renamedTriggers = i.triggers.map(_.map(x => Rename(x, x.scope.resolveVar("this").get, newThis)))

    val (guard, call) = genInvariantCall(c, i, "tid", thisName, heapPrefix, heapSuffix)
    val text = s"${guard} (forall _this : ${c.name} :: ${renamedTriggers.map(_.map(pp(_)).mkString(" { ", ", ", " } ")).mkString("")} { ${c.name}._state[_this] } isAccessible(${c.name}._state[_this], tid) && ${avoidThis} ==> ${call})"
    text
  }

  private def genMethodSpec(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, x: MethodDecl) = {
    val spec = x.spec
    if (spec.hasEnsures()) {

      val params = List("""tid:Tid""", s"this : ${x.parent.name}") ++ (x.params).map(p =>s"${p.name} : ${pp(p.t)}") ++
      (x.spec.vars.map(p =>s"${p.name} : ${pp(p.t)}")) ++
        (x.spec.vars.map(p =>s"${p.name}${this.oldSuffix} : ${pp(p.t)}"))

        buffer.emit(
          s"""
             | function {:inline} ${specFunctionPrefix(x)}.isSkip(${params.mkString("", ",", ",")} ${(genHeapParameterList("", oldSuffix) ++ genHeapParameterList()).mkString(",")}): bool {
             | @ ${(specStateEquivalencePredicates(buffer, ctxt, "", oldSuffix)++
                    x.spec.vars.filter(_.name != "$result").filter(!_.name.startsWith("spec")).map(v => s"(${v.name}${oldSuffix} == ${v.name})")).mkString(s"\n   && ")}
             | }
            """.stripMargin)

      for ((t,i) <- spec.transactions.zipWithIndex) {
        val name = s"${specFunctionPrefix(x)}.${i}"
        val ensures = t.ensures.map(e => s"(${this.pp(e, ExprContext(ctxt, false, "", this.oldSuffix))})") ++ specStateEquivalencePredicates(buffer, ctxt,"", oldSuffix, t.modifies)
        buffer.emit(s"""function {:inline} ${name}(${params.mkString("", ",", ",")} ${(genHeapParameterList("", this.oldSuffix) ++ genHeapParameterList()).mkString(",")}): bool {
            | @ ${ensures.mkString("\n && ")}
            }""".stripMargin)
      }
      atomicSpecTransition(buffer, x)
    }
  }

  def specFunctionPrefix(x:MethodDecl) = s"${this.methodSpecPrefix}.${x.parent.name}.${x.name}.IsValidTransition"

  def stateInSpecTransition(spec: ExplicitMethodSpec, i : Int) = {
    val into = spec.transitionsInto(i)
    (s"(${spec.specStateVarName(i)} && skips)" +: into.map(x => s"(${spec.specStateVarName(x._1)} && a${x._2})")).mkString(" || ")
  }

  private def atomicSpecTransition(buffer: AnnotatedBuffer, x: MethodDecl): Unit = {
    val spec = x.spec
    val name = s"${specFunctionPrefix(x)}"
    val params = List("tid:Tid", s"this : ${x.parent.name}") ++ (x.params).map(p =>s"${p.name} : ${pp(p.t)}") ++
    (x.spec.vars.map(p =>s"${p.name} : ${pp(p.t)}")) ++
         (x.spec.vars.map(p =>s"${p.name}${this.oldSuffix} : ${pp(p.t)}"))

    val nums = 0 until x.spec.specSize
    val ts = (0 until x.spec.transactions.size)
    val transition = s"""
      |    procedure ${x.parent.name}.${x.name}.specTransition(${spec.specStateVarDecls().map(v=>s"${v.name} : ${pp(v.t)}").mkString(",")}, ${params.mkString("", ",", ",")} ${(genHeapParameterList("", this.oldSuffix) ++ genHeapParameterList()).mkString(",")}) returns (${spec.specStateVarDeclTuple("", "$new")});
      |    @  ensures (var skips${ts.map(i => s"a${i}").mkString(",", ",", "")} :=
      |    @@           ${name}.isSkip(${params.mkString("", ",", ",")} ${(genHeapArgList("", this.oldSuffix) ++ genHeapArgList()).mkString(",")})
      |               ${ts.map(i=>s"${name}.${i}(${params.mkString("", ",", ",")} ${(genHeapArgList("", this.oldSuffix) ++ genHeapArgList()).mkString(",")})").mkString(s",\n@@", s",\n@@", "\n@@")};
      |    @   ${nums.map(i=>s"(${spec.specStateVarName(i, "", "$new")} == (${stateInSpecTransition(x.spec, i)}))").mkString("", "\n && ", "")}
      |       );
    """.stripMargin
    buffer.emit(transition)
  }


  private def declsAndVarsForMethodSpec(ctxt: SinkStmtContext, suffix: String) = {
    if (ctxt.topIsPublic && options.checkMethodSpec && ctxt.method.get.spec.hasEnsures()) {
      (ctxt.method.get.spec.specStateVarDecls("", suffix) ++
        ctxt.method.get.spec.specLocalVarDecls(this.methodSpecPrefix, suffix)
        ,
        ctxt.method.get.spec.specStateVarNames() ++
          ctxt.method.get.spec.specLocalVarDecls(this.methodSpecPrefix, "").map(_.name)
      )
    } else {
      (Set(), Set())
    }
  }

  /////

  private def genCooperative(buffer: AnnotatedBuffer, x: MethodDecl) = {

    val ctxt = new SinkStmtContext(x)

    def genSignature(codeName: String) = {
      val params = new AnnotatedBuffer(", ")
      params.emit("""tid:Tid""")
      params.emit(s"this : ${x.parent.name}")
      for (p <- x.params) {
        params.emit(s"${p.name} : ${pp(p.t)}")
      }

      buffer.emit(s"procedure  ${codeName}(", params, ")")

      x.returnType match {
        case VoidType() =>
        case t => buffer.emit(s"returns ($$result : ${pp(t)})")
      }

      // Modifies

      buffer.emit(genModifiesList())

      // Requires
      buffer.emit()
      buffer.emit("", genHeapInvariants(x, "requires"), "")

      buffer.emit()
      buffer.emit(s"requires ${genStateInvariantCall()};")

      for (require <- x.spec.requires) {
        buffer.emit(s"requires ${pp(require)};")
        buffer.emit();
      }

      genGlobalInvariants(buffer, ctxt, InvariantPrefix.requires, x)

      // Ensures

      buffer.emit()
      buffer.emit(s"ensures ${genStateInvariantCall()};")

      genGlobalInvariants(buffer, ctxt, InvariantPrefix.ensures, x)
    }

    buffer.annotate(new MethodAnnotation(x)) {

      methodSpecCheck {
        genMethodSpec(buffer, ctxt, x)
      }

      val codeName = s"${x.parent.name}.${x.name}"

      // Consistency Check
      if (options.checkConsistency) {
        genSignature(codeName + ".CheckConsistency")
        buffer.emit("{ assert false; }", InconsistentError(x, "Method has inconsistent requirements."))
        buffer.emit()
        buffer.emit()
      }

      val body = new AnnotatedBuffer()
      val postCtxt = gen(body, ctxt, x.stmt)
      val vars = postCtxt.locals ++ x.spec.vars.filter(_.name != "$result")


      buffer.emit()
      buffer.emit()

      // Normal
      genSignature(codeName)
      buffer.emit("{")
      buffer.push()

      // all locals
      buffer.emit(genLocals(vars))

      val spec = x.spec
      methodSpecCheck {
        buffer.emit(
          s"""
             | ${genHeapParameterList(this.methodSpecPrefix).map(x => s"var ${x};").mkString("\n")}
             | ${spec.specStateVarDecls().map(v => s"var ${v.name} : ${pp(v.t)}; ").mkString("")}
             | ${spec.specLocalVarDecls(this.methodSpecPrefix, "").map(v => s"var ${v.name} : ${pp(v.t)}; ").mkString("")}
          """.stripMargin)
      }

      reductionCheck {
        buffer.emit()
        buffer.emit("var _pc : Phase;")
      }

      methodSpecCheck {
        buffer.emit(
          s"""
             | ${genHeapArgList().map(x => s"${this.methodSpecPrefix}${x} := ${x};").mkString("\n")}
             | ${methodSpecCheckOrElse { spec.specStateVarDecls().zipWithIndex.map(v => s"${v._1.name} := ${if (v._2 == 0) "true" else "false" };").mkString(" ")} {""} }
          """.stripMargin)
      }

      reductionCheck {
        buffer.emit("_pc := PreCommit;")
        buffer.emit()
      }

      for (e <- x.spec.requires) {
        this.genRequiresCheck(buffer, e)
      }
      buffer.emit()

      buffer.emit(body)
      buffer.pop()
      buffer.emit("}")
      buffer
    }
  }



  def genLocals(vars: Set[VarDecl]): String = {
    (for (l <- vars) yield {
      s"var ${l.name}: ${pp(l.t)};\n"
    }).mkString
  }

  def genParams(vars: List[VarDecl]): String = {
    (for (l <- vars) yield {
      s"${l.name}: ${pp(l.t)}"
    }).mkString(", ")
  }

  private def gen(buffer: AnnotatedBuffer, x: MethodDecl): Unit = {
    assert(x.isPublic)
    genCooperative(buffer, x)
  }

  def genModifiesList() = {
    var list = List[String]()
    for (c <- classes) {
      list = list :+ s"modifies ${c.name}._state;"
      for (f <- c.fields) {
        list = list :+ s"modifies ${c.name}.${f.name};"
      }
      for (array <- c.arrays) {
        val name = s"Array.${array.parent.name}.${array.name}"
        list = list :+ s"modifies ${name}._state;"
        list = list :+ s"modifies ${name}._elems;"
      }
    }
    list.mkString("", "\n", "\n")
  }


    // checkCooperative only
  private def genMover(ctxt: SinkStmtContext, buffer: AnnotatedBuffer, v: VarAccess, x: Stmt): SinkStmtContext = {
      buffer.emit(s"_pc := transition(_pc, ${pp(v)});")
      buffer.emit(s"assert _pc != PhaseError;", ReductionError(x, s"Reduction failure"))
      ctxt
  }


  private def newHoudiniVar(x: Stmt) = {
    s"houdini_${x.id}"
  }

  private def newPhaseTemp(x: Stmt) = {
    val decl = VarDecl(BoogieType("Phase"), s"phase${x.id}")
    val access = new VarAccess(decl)
    access.pos = x.pos
    (decl, access)
  }

  private def newSpecPCTemp(x: Stmt) = {
    val decl = VarDecl(BoogieType("int"), s"${this.methodSpecPrefix}pc${x.id}")
    val access = new VarAccess(decl)
    access.pos = x.pos
    (decl, access)
  }

  private def newMoverTemp(x: Stmt) = {
    val decl = VarDecl(BoogieType("Mover"), s"mover${x.id}")
    val access = new VarAccess(decl)
    access.pos = x.pos
    (decl, access)
  }

  private def newPathTemp(x: Stmt) = {
    val decl = VarDecl(BoogieType("int"), s"path${x.id}")
    val access = new VarAccess(decl)
    access.pos = x.pos
    (decl, access)
  }

  private def newMoverPathTemp(x: Stmt) = {
    val decl = VarDecl(BoogieType("MoverPath"), s"moverPath${x.id}")
    val access = new VarAccess(decl)
    access.pos = x.pos
    (decl, access)
  }

  def moverOnly(moverPath: String): String = {
    s"m#moverPath($moverPath)"
  }

  def pathOnly(moverPath: String): String = {
    s"p#moverPath($moverPath)"
  }

  private def genInvariantAssertsForAfterStmt(buffer: AnnotatedBuffer, s: Stmt, invariants: List[Expr]): Unit = {
    s match {
      case VarDeclStmt(_)
           | Assign(_, _)
           | Write(_, _, _, _)
           | Read(_, _, _, _)
           | AWrite(_, _, _)
           | ARead(_, _, _)
           | CAS(_, _, _, _, _)
           | Invoke(_, _, _, _, _)
           | Alloc(_, _)
           | AAlloc(_, _, _)
           | Yield(_)
           | BoogieCode(_)
           | Acquire(_)
           | Release(_) => {
        for (inv <- Modifies.killLocalInvariants(s, invariants)) {
          buffer.emit(s"assert ${pp(inv)};", InvariantError(s, inv, s"Invariant may not hold."))
        }
      }
      case _            =>
    }
  }

  private var suffixesUsed : Set[String] = Set.empty

  def stateSuffixForStmt(x: Stmt, post: String = "") = {
    val r = s"${x.id}" + post
    assert(!suffixesUsed.contains(r), "Two state preservation calls with same node id")
    suffixesUsed += r
    r
  }

  def genMoverDeclsAndComputation(buffer: AnnotatedBuffer, ctxt: SinkStmtContext,
                                  x: Stmt, eval: String, preChecks: => Unit, moverOverride : Option[Mover] = None) = {
    val (moverPathTemp, moverPathAccess) = newMoverPathTemp(x)
    val (moverTemp, moverAccess) = newMoverTemp(x)
    val (pathTemp, pathAccess) = newPathTemp(x)
    buffer.emit(s"${pp(moverPathAccess)} := $eval;")
    buffer.emit(s"${pp(moverAccess)} := ${moverOnly(pp(moverPathAccess))};")
    buffer.emit(s"${pp(pathAccess)} := ${pathOnly(pp(moverPathAccess))};")
    val (decls, code) = preserveState(x.scope, stateSuffixForStmt(x), ctxt = ctxt)
    buffer.emit(code)
    preChecks
    moverOverride match {
      case Some(value) => {
        buffer.emit(s"_pc := transition(_pc, ${pp(value)});")
        ctxt
      }
      case None        => {
        buffer.emit(s"_pc := transition(_pc, ${pp(moverAccess)});")
      }
    }
    buffer.emit(s"assert _pc != PhaseError;", ReductionError(x, s"Reduction failure"))
    ctxt addVars Set(moverPathTemp, moverTemp, pathTemp) addVars decls
  }

  private def gen(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, x: Stmt): SinkStmtContext = {

    def annotate(f: => SinkStmtContext, stmt: Stmt = x): SinkStmtContext = {
      val a = new StmtAnnotation(stmt)
      buffer.emit()
      buffer.annotate(a) {
        buffer.emit(s"// ${stmt.pos}: ${PrettyPrint.pp(stmt).split("\n")(0)}")
        buffer.emit()
        f
      }
    }

    val ctxt2 = x match {
      case VarDeclStmt(v) => {
        annotate {
          ctxt addVar v
        }
      }

      case Assign(lhss, rhss) => {
        annotate {
          for ((lhs, rhs) <- lhss.zip(rhss)) {
            if (rhs == Rand()) {
              buffer.emit(s"havoc ${pp(lhs)};")
            } else {
              buffer.emit(s"${pp(lhs)} := ${pp(rhs)};")
            }
          }
          ctxt
        }
      }

      case InlineInvoke(invoke) => {
        buffer.emit(s"// inlined: ${PrettyPrint.pp(invoke.ref)}.${invoke.method}(${invoke.args.map(PrettyPrint.pp(_)).mkString(",")})}")
        ctxt.pushScope(Some(invoke))
      }

      case InlineReturn() => {
        ctxt.popScope()
      }

      case Block(name, body) => {
        val newCtxt = ctxt.pushScope(None)
        val result = name match {
          case None       =>
            body.foldLeft(newCtxt)((ctxt, s) => gen(buffer, ctxt, s))
          case Some(name) => {
            buffer.emit(s"${name}_top:")
            val ctxt2 = body.foldLeft(newCtxt)((ctxt, s) => gen(buffer, ctxt, s))

            buffer.emit(s"${name}_bottom:")
            ctxt2
          }
        }
        result.popScope()
      }

      case x@Write(lhs, field, rhs, movesAs) => {
        val c = x.decl.parent
        def preChecks = {
          genNotNullBlockingOperation(buffer, x, x.ref)
        }
        buffer.emit()

        annotate {
          buffer.emit()
          val newCtxt = reductionCheckOrElse {
            val eval = genWriteEvalCall(x.decl, x.ref.name, "tid", pp(x.rhs), false)
            genMoverDeclsAndComputation(buffer, ctxt, x, eval, preChecks, movesAs)
          } {
            preChecks
            ctxt
          }
          buffer.emit(s"${c.name}.${x.decl.name}[${pp(x.ref)}] := ${pp(x.rhs)};")
          if (!x.decl.isInternal) {
            localShare(buffer, x, x.rhs)
          }
          newCtxt
        }
      }

      case x@LocalWrites(writes) => {
        annotate {
          buffer.emit(s"assert _pc == PreCommit;", ReductionError(x, s"Local Write block must appear pre commit."))
          val newCtxt = writes.foldLeft(ctxt)((ctxt, x) => {
            val c = x.decl.parent

            def preChecks = {
              genNotNullBlockingOperation(buffer, x, x.ref)
              buffer.emit()
            }

            annotate({
              buffer.emit()
              val newCtxt = reductionCheckOrElse {
                val eval = genWriteEvalCall(x.decl, x.ref.name, "tid", pp(x.rhs), false)
                val ctxt2 = genMoverDeclsAndComputation(buffer, ctxt, x, eval, preChecks)
                buffer.emit(s"assert _pc == PreCommit;", ReductionError(x, s"Local Write block can only include right-movers."))
                ctxt2
              } {
                preChecks
                ctxt
              }
              buffer.emit(s"${c.name}.${x.decl.name}[${pp(x.ref)}] := ${pp(x.rhs)};")
              newCtxt
            }, x)
          })
          val checks = for (x <- writes if !x.decl.isInternal) yield {
            localShareDeferredChecks(buffer, x, x.rhs)
          }
          for (x <- writes if !x.decl.isInternal) yield {
            shareNoChecks(buffer, x, x.rhs)
          }
          for (c <- checks) {
            c()
          }
          newCtxt
        }
      }

      case x@Read(lhs, ref, field, movesAs) => {
        val c = x.decl.parent
        def preChecks = {
          genNotNullBlockingOperation(buffer, x, x.ref)
          buffer.emit()
        }

        annotate {
          buffer.emit()
          val newCtxt = reductionCheckOrElse {
            val eval = genReadEvalCall(x.decl, ref.name, "tid", false)
            genMoverDeclsAndComputation(buffer, ctxt, x, eval, preChecks, movesAs)
          } {
            preChecks
            ctxt
          }
          buffer.emit(s"${pp(lhs)} := ${c.name}.${field}[${pp(ref)}];")
          newCtxt
        }
      }

      case x@CAS(result, ref, field, expected, rhs) => { fail("SinkPrinter", "Should not get CAS here", x) }

      case x@While(cond, stmt, invs, decreases) => {

        val yieldsInsideLoop = yields(x)

        val (headPhaseDecl, headPhaseAccess) = this.newPhaseTemp(x)
        val methodSpec = ctxt.method.get.spec
        val headSpecPCDecls = methodSpecCheckOrElse { methodSpec.specStateVarDecls("", s"$$loopHead${x.id}") } { Nil }

        val decreaseDecls = (for (n <- 0 until decreases.length) yield {
          List(VarDecl(IntType(), s"$$decr$$init$$${x.id}$$$n"),
            VarDecl(IntType(), s"$$decr$$loop$$${x.id}$$$n"))
        }).flatten

        val (decls,code) = preserveState(x.scope, stateSuffixForStmt(x), ctxt = ctxt)
        buffer.emit(code)
        val ctxtWithPreserved = ctxt addVars (decls ++ decreaseDecls ++ headSpecPCDecls + headPhaseDecl)

        annotate {
          reductionCheck {
            buffer.emit(s"${headPhaseAccess.name} := _pc;")
          }
          methodSpecCheck {
            if (yieldsInsideLoop) {
              for (v <- allStateVariables()) {
                buffer.emit(s"${this.methodSpecPrefix}${v._2} := ${v._2};")
              }
            }
            for (v <- methodSpec.specLocalVarDecls()) {
              buffer.emit(s"${this.methodSpecPrefix}${v.name} := ${v.name};")
            }
            val specStateVars = methodSpec.specStateVarNames()
            for (v <- headSpecPCDecls.map(_.name).zip(specStateVars)) {
              buffer.emit(s"${v._1} := ${v._2};")
            }
            if (specStateVars.size > 0) {
              buffer.emit(s"assert ${specStateVars.mkString(" || ")};", MethodSpecError(x, ctxt.method.get, "Cannot construct possible Spec States for loop head."))
            }

          }
          for ((e,n) <- decreases.zipWithIndex) {
            buffer.emit(s"$$decr$$init$$${x.id}$$$n := ${pp(e)};")
          }
          buffer.emit(s"while (${pp(cond)})")
          ctxtWithPreserved
        }

        buffer.push()
        buffer.emit()
        buffer.emit("", genHeapInvariants(ctxtWithPreserved.method.get, "invariant"), "")
        buffer.emit()
        buffer.emit(s"invariant ${genStateInvariantCall()};")
        genGlobalInvariants(buffer, ctxtWithPreserved, InvariantPrefix.invariant, x, Some(stmt))
        for (inv <- invs ++ ctxtWithPreserved.invariants) {
          buffer.annotate(InvariantError(x, inv, s"invariant ${PrettyPrint.pp(inv)} may not hold")) {
            buffer.emit(s"invariant ${pp(inv, ExprContext(ctxtWithPreserved, false, "", ""))};")
          }
        }

        if (decreases.size > 0) {
          val decreasingInv = decreases.zipWithIndex.foldRight("true")((d: (Expr, Int), r: String) => {
            val init = s"$$decr$$init$$${x.id}$$${d._2}"
            val e = pp(d._1)
            s"($e <= $init && ($e == $init ==> $r))"
          })

          buffer.annotate(LoopTerminationError(x, None, "decreasing expressions may actually increase")) {
            buffer.emit(s"invariant $decreasingInv;")
          }
        }

        reductionCheck {
          buffer.emit(s"invariant ${headPhaseAccess.name} == _pc;", ReductionError(x, "Phase must be invariant at loop head"))
          if (decreases.size == 0) {
            buffer.emit("invariant _pc == PreCommit;", ReductionError(x, "Potentially infinite loop head cannot be in post-commit phase."))
          }
        }
        methodSpecCheck {
          if (yieldsInsideLoop) {
            for (v <- allStateVariables()) {
              buffer.emit(s"invariant ${this.methodSpecPrefix}${v._2} == ${v._2};")
            }
          } else {

            genSpecModifiesInvariants(buffer, ctxt, methodSpec)
          }
          for (v <- methodSpec.specLocalVarDecls()) {
            buffer.emit(s"invariant ${this.methodSpecPrefix}${v.name} == ${v.name};")
          }
          if (methodSpec.transactions.size == 1) {
            buffer.emit(s"invariant ${methodSpec.stateForNextStep(0)};")
          }

//          for (v <- methodSpec.specStateVarDecls().zip(headSpecPCDecls)) {
//            buffer.emit(s"invariant ${v._1.name} == ${v._2.name};", MethodSpecError(x, ctxt.method.get, s"Spec PC  ${v._1.name} must be invariant at loop head"))
//          }

        }
        buffer.pop()
        buffer.emit(s"{")
        buffer.push()

        for ((e,n) <- decreases.zipWithIndex) {
          buffer.emit(s"$$decr$$loop$$${x.id}$$$n := ${pp(e)};")
        }

        val ctxt2 = gen(buffer, ctxtWithPreserved, stmt)

        if (decreases != Nil) {
          for (n <- 0 until decreases.length) {
            val decreasingAssert = decreases.zipWithIndex.foldRight("false")((d: (Expr, Int), r: String) => {
              val loop = s"$$decr$$loop$$${x.id}$$${d._2}"
              val e = pp(d._1)
              s"($e <= $loop && ($e == $loop ==> $r))"
            })
            buffer.emit(s"assert $decreasingAssert;", new LoopTerminationError(x, None,"loop may not terminate"))

            val clauses = List(s"0 <= $$decr$$loop$$${x.id}$$$n ") ++
              ((0 until n).map(i => s"${pp(decreases(i))} < $$decr$$loop$$${x.id}$$$i")) ++
              List(s"${pp(decreases(n))} == $$decr$$loop$$${x.id}$$$n")
            buffer.emit(s"assert ${clauses.mkString(" || ")};", new LoopTerminationError(x, Some(decreases(n)), "decreasing expression not properly bounded by 0"))
          }

        }

        val (decls2,code2) = preserveState(x.scope, stateSuffixForStmt(x, "_bottom"), ctxt = ctxt)
        buffer.emit(code2)
        reductionCheck {
          buffer.emit(s"assert ${headPhaseAccess.name} == _pc;", ReductionError(x, "Phase must be invariant at loop head"))
        }
        methodSpecCheck {
          if (yieldsInsideLoop) {
            for (v <- allStateVariables()) {
              buffer.emit(s"${this.methodSpecPrefix}${v._2} := ${v._2};")
            }
          }
          for (v <- methodSpec.specLocalVarDecls()) {
            buffer.emit(s"${this.methodSpecPrefix}${v.name} := ${v.name};")
          }




        }

        buffer.pop()
        buffer.emit("}")
        ctxt2 addVars decls2
      }

      case x@If(cond, t, f) => {
        buffer.emit(s"if (${pp(cond)}) {")
        buffer.push()
        val tCtxt = gen(buffer, ctxt, t)
        buffer.pop()
        buffer.emit("} else {")
        buffer.push()
        val fCtxt = gen(buffer, ctxt, f)
        buffer.pop()
        buffer.emit("}")
        ctxt.addVars(tCtxt.locals -- ctxt.locals).addVars(fCtxt.locals -- ctxt.locals)
      }

      case Sync(lock, stmt, _) => {
        error("SinkPrinter", "Can't have synch stmt in Sink Printer")
        ctxt
      }

      case Break(label) => {
        annotate {

          label match {
            case None        => buffer.emit("break;")
            case Some(label) => buffer.emit(s"goto ${label}_bottom;")
          }
          ctxt
        }
      }

      case stmt@Return(result, _) => {
        annotate {

          val (decls,code) = preserveState(x.scope, stateSuffixForStmt(x), ctxt = ctxt)
          buffer.emit(code)
          val ctxtWithPreserved = ctxt addVars decls

          // TODO: exits and ensures
          result match {
            case None    =>
            case Some(x) => {
              localShare(buffer, stmt, x)
              buffer.emit(s"$$result := ${pp(x)};")
            }
          }

          // do this here because we want to report when invariant doesn't hold, but
          // if we leave it to the ensures checks, we can't recover a meaningful error message.
          genGlobalInvariants(buffer, ctxtWithPreserved, InvariantPrefix.assert, x)

          val moreDecls = methodSpecCheckOrElse {
            genSpecTransition(buffer, ctxt)

            val (decls2,code2) = preserveState(x.scope, stateSuffixForStmt(x, "_post"), ctxt = ctxt)
            buffer.emit(code2)

            genSpecAssertAcceptState(buffer, ctxt, stmt)
            decls2
          } { Set() }


          buffer.emit("return;")
          ctxtWithPreserved addVars moreDecls
        }
      }

      case Alloc(lhs, name) => {
        annotate {
          val c = name.decl
          buffer.emit(s"havoc ${pp(lhs)};")
          buffer.emit(s"assume ${pp(lhs)} != ${c.name}.null && isFresh(${c.name}._state[${pp(lhs)}]);")
          methodSpecCheck {
            buffer.emit(s"assume isFresh(${methodSpecPrefix}${c.name}._state[${pp(lhs)}]);")
          }
          buffer.emit(s"${c.name}._state[${pp(lhs)}] := LOCAL(tid);")
          for (f <- c.fields if !f.isInternal) {
            buffer.emit(s"assume ${c.name}.${f.name}[${pp(lhs)}]  == ${pp(ConstExpr(defaultValue(f.t)))};")
          }
          ctxt
        }
      }

      case x@Assert(e) => {
        annotate {
          val (decls,code) = preserveState(x.scope, stateSuffixForStmt(x), ctxt = ctxt)
          buffer.emit(code)
          buffer.emit(s"assert ${pp(e)};", AssertError(x, e, s"This assertion may not hold."))
          ctxt addVars decls
        }
      }

      case Assume(e) => {

        annotate {
          buffer.emit(s"assume ${pp(e, ExprContext(ctxt, false, "", ""))};")
          ctxt
        }
      }

      case x@Yield(ensures) => {
        if (!options.yeildsAreNoOps) {
          annotate {
            val (decls,code) = preserveState(x.scope, stateSuffixForStmt(x, ""), ctxt = ctxt)
            buffer.emit(code)


            genSpecTransition(buffer, ctxt)

            genGlobalInvariants(buffer, ctxt, InvariantPrefix.assert, x)
            buffer.emit(s"call Yield(tid);")
            genGlobalInvariants(buffer, ctxt, InvariantPrefix.assume, x)

            for (e <- ensures) {
              buffer.emit(s"assert ${pp(e, ExprContext(ctxt, false, "", "" + x.id))};", EnsuresError(x, e, s"Ensures expression ${PrettyPrint.pp(e)} may not hold"))
            }

            methodSpecCheck {
              buffer.emit(genHeapArgList().map(x => s"${this.methodSpecPrefix}${x} := ${x};").mkString(""))
              for (v <- ctxt.method.get.spec.specLocalVarDecls()) {
                buffer.emit(s"${this.methodSpecPrefix}${v.name} := ${v.name};")
              }
            }


            reductionCheck {
              buffer.emit("_pc := PreCommit;")
            }
            val (decls2,code2) = preserveState(x.scope, stateSuffixForStmt(x, "_post"), ctxt = ctxt)
            buffer.emit(code2)

            methodSpecCheck {
              val method = ctxt.method.get
              val specVars = method.spec.specStateVarNames()
              if (specVars.size > 0) {
                buffer.emit(s"assert ${specVars.mkString(" || ")};", MethodSpecError(x, method, s"Atomic block is not pure and does not conform to spec"))
              }
            }

            ctxt addVars (decls ++ decls2)
          }
        } else {
          reductionCheck {
            buffer.emit("_pc := PreCommit;")
          }
          ctxt
        }
      }

      case x@Commit() => {
        val (decls,code) = preserveState(x.scope, stateSuffixForStmt(x), ctxt = ctxt)
        buffer.emit(code)
        val ctxtWithPreserved = ctxt addVars decls

        reductionCheck {
          buffer.emit("assert _pc == PreCommit;", ReductionError(x, "Can only commit when in pre-commit phase."))
          buffer.emit("_pc := PostCommit;")
        }
        ctxtWithPreserved
      }

      case x@BoogieCode(s) => {
        annotate {
          buffer.emit("/* ## */ " + s, InlineBoogieError(x, "Inline Boogie code failure."))
          ctxt
        }
      }

      case AAlloc(lhs, t, size)     => {
        annotate {
          val name = pp(t)
          buffer.emit(s"havoc ${pp(lhs)};")
          buffer.emit(s"assume isFresh(${name}._state[${pp(lhs)}]);")
          methodSpecCheck {
            buffer.emit(s"assume isFresh(${methodSpecPrefix}${name}._state[${pp(lhs)}]);")
          }
          buffer.emit(s"assume ${name}._length[${pp(lhs)}] == ${pp(size)};")
          buffer.emit(s"assume ${name}._this[${pp(lhs)}] == ${pp(t.thisAccess)};")
          buffer.emit(s"${name}._state[${pp(lhs)}] := LOCAL(tid);")
       //   buffer.emit(s"assume (forall _i : int :: 0 <= _i && _i < ${pp(size)} ==> ${name}._elems[${pp(lhs)}][_i] == ${pp(ConstExpr(defaultValue(t.elemType())))});")
          buffer.emit(s"assume (forall _i : int :: 0 <= _i ==> ${name}._elems[${pp(lhs)}][_i] == ${pp(ConstExpr(defaultValue(t.elemType())))});")
          ctxt
        }
      }
      case x@ARead(lhs, rhs, index) => {
        val name = pp(rhs.t)
        def preChecks = {
          genNotNullBlockingOperation(buffer, x, rhs)
          genAssumeOrAssert(buffer, s"0 <= ${pp(index)}", Some(ReductionError(x, "index < 0.")))
          genAssumeOrAssert(buffer, s"${pp(index)} < ${name}._length[${pp(rhs)}]", Some(ReductionError(x, "index is >= length.")))
          buffer.emit()
        }

        annotate {
          buffer.emit()
          val arrayType = rhs.t.asInstanceOf[ArrayType]
          val newCtxt = reductionCheckOrElse {
            val eval = genReadEvalCall(arrayType.decl, pp(arrayType.thisAccess), rhs.name, "tid", index.name, false)
            genMoverDeclsAndComputation(buffer, ctxt, x, eval, preChecks)
          } {
            preChecks
            ctxt
          }
          buffer.emit(s"${pp(lhs)} := ${name}._elems[${pp(rhs)}][${pp(index)}];")
          newCtxt
        }
      }

      case x@AWrite(lhs, index, rhs) => {
        val name = pp(lhs.t)
        def preChecks = {
          genNotNullBlockingOperation(buffer, x, lhs)
          genAssumeOrAssert(buffer, s"0 <= ${pp(index)}", Some(ReductionError(x, "index < 0.")))
          genAssumeOrAssert(buffer, s"${pp(index)} < ${name}._length[${pp(lhs)}]", Some(ReductionError(x, "index is >= length.")))
          buffer.emit()
        }

        annotate {
          buffer.emit()
          val newCtxt = reductionCheckOrElse {
            val arrayType = lhs.t.asInstanceOf[ArrayType]
            val eval = genWriteEvalCall(arrayType.decl, pp(arrayType.thisAccess), lhs.name, "tid", index.name, pp(rhs), false)
            genMoverDeclsAndComputation(buffer, ctxt, x, eval, preChecks)
          } {
            preChecks
            ctxt
          }
          buffer.emit(s"${name}._elems[${pp(lhs)}][${pp(index)}] := ${pp(rhs)};")
          localShare(buffer, x, rhs)
          newCtxt
        }
      }

      case x@Acquire(lock) => {
        // fail("SinkPrinter", "Should not get acquire here", x)
        genNotNullBlockingOperation(buffer, x, lock)

        buffer.emit(s"assume ${pp(lock.t)}._lock[${pp(lock)}] == Tid.null;")

        reductionCheck {
          buffer.emit(s"_pc := transition(_pc, _R);")
          buffer.emit(s"assert _pc != PhaseError;", ReductionError(x, s"Reduction failure"))
        }

        buffer.emit(s"${pp(lock.t)}._lock[${pp(lock)}] := tid;")

        ctxt
      }
      case x@Release(lock) => {
        genNotNullBlockingOperation(buffer, x, lock)

        buffer.emit(s"assert ${pp(lock.t)}._lock[${pp(lock)}] == tid;", ReleaseError(x, "lock not held"))

        reductionCheck {
          buffer.emit(s"_pc := transition(_pc, _L);")
          buffer.emit(s"assert _pc != PhaseError;", ReductionError(x, s"Reduction failure"))
        }

        buffer.emit(s"${pp(lock.t)}._lock[${pp(lock)}] := Tid.null;")
        ctxt
      }
        //fail("SinkPrinter", "Should not get release here", x) }

      case x : Invariant          =>
        {
          buffer.annotate(InvariantError(x, x.expr, "invariant may not hold")) {
            buffer.emit(s"assert ${pp(x.expr)};")
          }
          ctxt addInv x.expr
        }
      case NoReductionCheck(stmt) => {
        noReductionCheck {
          buffer.emit("// nocheck { ")
          buffer.emit()
          buffer.push()
          val ctxt2 = this.gen(buffer, ctxt, stmt)
          buffer.pop()
          buffer.emit()
          buffer.emit("// } ")
          ctxt2
        }
      }
      case x                      => {
        buffer.emit(s"// ${x}")
        //        assert(false, s"Unhandled case ${x}")
        ctxt
      }
    }
    genInvariantAssertsForAfterStmt(buffer, x, ctxt2.invariants)
    ctxt2
  }


  private def genSpecAssertAcceptState(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, stmt: Return) = {
    val method = ctxt.method.get
    if (method.spec.hasEnsures()) {
      buffer.emit(s"assert ${method.spec.isInAcceptState()};", MethodSpecError(stmt, method, s"Method returns before completing actions in spec"))
    }
  }

  private def specStateEquivalencePredicates(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, prefix: String = "", suffix: String = "", modifies: List[Expr] = Nil): List[String] = {
    var list : List[String] = Nil
    def ruleOutMods(name: String, t: Type) = {
      val mods = modifies.filter(_.t == t).filter(_ != VarAccess("$result"))
      mods match {
        case Nil => "true"
        case _   => mods.map(v => s"${name} != ${pp(v, ExprContext(ctxt, true, prefix, suffix))}").mkString("&& ")
      }
    }
    for (c <- classes) {
      val mods = ruleOutMods("$this", ClassType(c.name))
      list = list :+ s"(forall $$this : ${c.name} :: $mods ==> (isShared(${prefix}${c.name}._state${suffix}[$$this]) ==> isShared(${c.name}._state[$$this])))"
      for (f <- c.fields if f.name != "_lock") {
        list = list :+ s"(forall $$this : ${c.name} :: $mods ==> (isShared(${prefix}${c.name}._state${suffix}[$$this]) ==> ${prefix}${c.name}.${f.name}${suffix}[$$this] == ${c.name}.${f.name}[$$this]))"
        if (Type.isHeapReference(f.t) && mods.length > 0) {
        }
      }
      for (a <- c.arrays) {
        val name = s"Array.${a.parent.name}.${a.name}"
        val mods = ("true" :: modifies.filter(m => pp(m.t) == name).map(v => s"$$this != ${pp(v, ExprContext(ctxt, true, prefix, suffix))}")).mkString("&& ")
        list = list :+ s"(forall $$this : ${name} :: $mods ==> (isShared(${prefix}${name}._state${suffix}[$$this]) ==> isShared(${name}._state[$$this])))"
        list = list :+ s"(forall $$this : ${name} ::  $mods ==> (isShared(${prefix}${name}._state${suffix}[$$this]) ==> ${prefix}${name}._elems${suffix}[$$this] == ${name}._elems[$$this]))"
      }
    }
    list
  }

  private def genSpecTransition(buffer: AnnotatedBuffer, ctxt: SinkStmtContext) = {
    methodSpecCheck {
      val x = ctxt.method.get
      if (x.spec.hasEnsures()) {
        val params = List("tid", "this") ++ (x.params).map(p => s"${p.name}") ++
          (x.spec.vars.map(p => s"${p.name} : ${pp(p.t)}")) ++
          (x.spec.vars.map(p => s"${this.methodSpecPrefix}${p.name} : ${pp(p.t)}"))

        val specVars = x.spec.specStateVarNameTuple()

        val specTransition =
          s"""
             | call ${specVars} :=
             |   ${x.parent.name}.${x.name}.specTransition(${specVars}, ${params.mkString("", ",", ",")} ${(genHeapArgList(this.methodSpecPrefix) ++ genHeapArgList()).mkString(",")});""".stripMargin
        buffer.emit(specTransition)
      }
    }
  }

  private def genSpecModifiesInvariants(buffer: AnnotatedBuffer, ctxt: SinkStmtContext, spec: ExplicitMethodSpec) = {
    for ((t, v) <- spec.transactions.zip(spec.specStateVarNames())) {
      val preds = specStateEquivalencePredicates(buffer, ctxt, this.methodSpecPrefix, "", t.modifies)
      for (p <- preds) {
        buffer.emit(s"invariant ${v} ==> (${p});")
      }
    }
  }


  private def genMoverEnsures(buffer: AnnotatedBuffer, f: FieldDecl) = {
    val c: ClassDecl = f.parent
    buffer.emit(s"// ${c.name}.${f.name}:")
    buffer.emit()
    val write = moverOnly(genWriteEvalCall(f, "this", "u", "_nu", true))
    val triggers = triggersAsString(f.spec, Set("this"))

    buffer.emit(
      s"""ensures (forall this : ${c.name} :: ${triggers} { ${c.name}.${f.name}[this] }
         |  (old(${c.name}.${f.name}[this]) != ${c.name}.${f.name}[this]) ==>
         |    (exists u : Tid :: ValidTid(u)
         |                    && u != tid
         |                    && isAccessible(${c.name}._state[this], u)
         |                    && ${moverOnly(genWriteEvalCall(f, "this", "u", s"${c.name}.${f.name}[this]", true))} != _E));
         |""".stripMargin)
    buffer.emit()
  }



  private def genMoverEnsures(buffer: AnnotatedBuffer, a: ArrayDecl) = {
    val enclosing = a.parent
    val name = s"Array.${a.parent.name}.${a.name}"
    val thisName = s"${name}._this[athis]"
    buffer.emit(s"// $name:")
    buffer.emit()
    val write = moverOnly(genWriteEvalCall(a, thisName, "athis", "u", a.elemName, s"${name}._elems[athis][${a.elemName}]", true))

    val triggers = triggersAsString(a.spec, Set("athis"))

    buffer.emit(
      s"""ensures (forall athis : ${name}, ${a.elemName} : int  :: /* ${triggers} */ { ${name}._elems[athis][${a.elemName}] }
         |    (0 <= ${a.elemName} && ${a.elemName} < ${name}._length[athis] &&
         |     old(${name}._elems[athis][${a.elemName}]) != ${name}._elems[athis][${a.elemName}])
         |  ==>
         |    (exists u : Tid :: ValidTid(u)
         |                    && u != tid
         |                    && isAccessible(${name}._state[athis], u)
         |                    && ${write} != _E));
         """.stripMargin)
    buffer.emit()
  }

  private def triggersAsString(x: Spec, vars: Set[String]) = {
    val triggers = this.triggers(x, vars)
    if (triggers.size == 0) "" else s"{ ${triggers.mkString(", ")} }"
  }

  private def triggers(x: Spec, vars: Set[String]): Set[String] = {
    def containsVar(x: Expr): Boolean = {
      x match {
        case ConstExpr(const)            => false
        case BinaryExpr(lhs, rhs, op)    => containsVar(lhs) || containsVar(rhs)
        case UnaryExpr(expr, op)         => containsVar(expr)
        case Cond(p, tt, ff)             => containsVar(p) || containsVar(tt) || containsVar(ff)
        case Quantified(_, decls, pred, triggers)         => containsVar(pred) // ignore triggers here?
        case location: Location          =>
          location match {
            case VarAccess(name)       => vars.contains(name)
            case FieldAccess(l, name)  => containsVar(l)
            case ArrayAccess(l, index) => containsVar(l) || containsVar(index)
          }
        case function: PrimitiveFunction =>
          function match {
            case Length(expr)              => containsVar(expr)
            case Lock(expr)              => containsVar(expr)
            case IsLocal(expr, tid)        => containsVar(expr)
            case Holds(expr, tid)          => containsVar(expr)
            case IsShared(expr)            => containsVar(expr)
            case IsFresh(expr)            => containsVar(expr)
            case MoverPermission(expr, None) => containsVar(expr)
            case MoverPermission(expr, Some(v)) => containsVar(expr) || containsVar(v)
            case GoesWrong(expr) => containsVar(expr)
            case Rand()                    => false
            case NextSpecStep(n) => false
          }
        case LoweredExpr(e, original)    => containsVar(e)
        case Old(l)                => containsVar(l)
        case BuiltInFunctionCall(name, types, args) => args.exists(containsVar(_))

      }
    }

    def etriggers(x: Expr): Set[String] = {
      x match {
        case ConstExpr(const)            => Set()
        case BinaryExpr(lhs, rhs, op)    => etriggers(lhs) ++ etriggers(rhs)
        case UnaryExpr(expr, op)         => etriggers(expr)
        case Quantified(_, decls,pred, triggers)          => etriggers(pred) // ignore triggers here?
        case Cond(p, t, f) => etriggers(p) ++ etriggers(t) ++ etriggers(f)
        case location: Location          => {
          location match {
            case VarAccess(name)                                  => Set()
            case FieldAccess(l, name) if (containsVar(location))  => Set(pp(location))
            case ArrayAccess(l, index) if (containsVar(location)) => Set(pp(location))
            case _                                                => Set()
          }
        }
        case function: PrimitiveFunction =>
          function match {
            case IsLocal(expr, t) if containsVar(expr)                           => {
              Set(s"${pp(expr.t)}._state[${pp(expr)}]") ++ etriggers(expr)
            }
            case Holds(expr, t) if containsVar(expr)                             => {
              Set(s"${pp(expr.t)}._lock[${pp(expr)}]") ++ etriggers(expr)
//              fail("SinkPrinter", "Should not get holds here", x)
            }
            case MoverPermission(l, v) if containsVar(l) || v.exists(containsVar(_)) => {
              etriggers(l) ++ (v match {
                case None => Set[String]()
                case Some(v) => etriggers(v)
              })
            }
            case GoesWrong(expr) if containsVar(expr) =>
              etriggers(expr)

            case _ => Set()
          }
        case LoweredExpr(e, original)    => etriggers(e)
        case Old(l)                     => etriggers(l)
        case BuiltInFunctionCall(name, types, args) => args.flatMap(etriggers(_)).toSet


      }
    }
    etriggers(x.conditionalMover)
  }




  /////////////////


  def genRequiresCheck(buffer: AnnotatedBuffer, x : Expr) = {
    def buildAssertionForRequires(x: Expr): List[String] = {
      x match {
        case ConstExpr(const)              => Nil
        case BinaryExpr(lhs, rhs, op)      => {
          buildAssertionForRequires(lhs) ++ buildAssertionForRequires(rhs)
        }
        case UnaryExpr(expr, op)           => {
          buildAssertionForRequires(expr)
        }
        case LoweredExpr(e, original)      => {
          buildAssertionForRequires(e)
        }
        case VarAccess(name)               => Nil
        case x@FieldAccess(l, name)   if name.startsWith("_")     => {
          buildAssertionForRequires(l)
        }
        case x@FieldAccess(l, name)        => {
          val eval = genReadEvalCall(x.decl, pp(l), "tid", false)
          s"leq(${moverOnly(eval)},_R)" :: buildAssertionForRequires(l)
        }
        case x@ArrayAccess(l, index)       => {
          val eval = genReadEvalCall(x.l.t.asInstanceOf[ArrayType].decl, pp(x.l.t.asInstanceOf[ArrayType].thisAccess), pp(l), "tid", pp(index), false)
          s"leq(${moverOnly(eval)},_R)" :: (buildAssertionForRequires(l) ++ buildAssertionForRequires(index))
        }
        case Old(l)                        => {
          fail("Requires", "Old cannot appear in requires.")
        }
        case Length(a)                     => {
          buildAssertionForRequires(a)
        }
        case IsLocal(loc, tid)             => {
          buildAssertionForRequires(loc)
        }
        case IsShared(loc)                 => {
          buildAssertionForRequires(loc)
        }
        case IsFresh(loc)                 => {
          buildAssertionForRequires(loc)
        }
        case Holds(loc, tid)               => {
          buildAssertionForRequires(loc)
        }
        case MoverPermission(loc, tid)     => {
          buildAssertionForRequires(loc)
        }
        case GoesWrong(expr) => {
          buildAssertionForRequires(expr)
        }
        case BuiltInFunctionCall(name, types, arguments) =>
          // arguments.map(buildAssertionForRequires(_).mkString(" && "))
          Nil

        case Rand()                        => Nil
        case Quantified(quantifier, decls, pred, triggers) => {
          val ts = triggers.map(_.map(pp(_)).mkString("{ ", ", ", " }")).mkString(" ")
          val q = quantifier match {
            case ForAll() => "forall"
            case Exists() => "exists"
          }
          List(s"(${q} ${decls.map(d => s"${d.name}: ${pp(d.t)}").mkString(",")} :: ${ts} ${("true"::buildAssertionForRequires(pred)).mkString(" && ")})")
        }
        case Cond(p, tt, ff)               => {
          buildAssertionForRequires(p) ++ buildAssertionForRequires(tt) ++ buildAssertionForRequires(ff)
        }
      }
    }
    buffer.emit(s"assert ${("true"::buildAssertionForRequires(x)).mkString(" && ")};", RequiresError(x, x, "Can only have right-mover memory accesses in requires clause"))
  }

  /////////////////

  case class ExprContext(val sinkContext: SinkStmtContext, val inOld : Boolean, val oldPrefix : String, val oldSuffix: String)

  def defaultValue(x: Type): Const = {
    x match {
      case IntType()            => IntConst(0)
      case BoolType()           => BoolConst(false)
      case x@ClassType(_)       => NullConst(x)
      case x@ArrayType(_, _, _) => NullConst(x)
      case TidType()            => NullTid()
      case x@CollectionType(_, _) => EmptyCollectionConst(x)
      case _                    => assert(false); IntConst(0)
    }
  }

  def pp(x: Const): String = {
    x match {
      case IntConst(v)  => v.toString
      case BoolConst(v) => v.toString
      case NullConst(t) => s"${pp(t)}.null"
      case NullTid()    => "Tid.null"
      case MoverConst(m) => pp(m)
      case EmptyCollectionConst(t) => s"${t.name}Empty.${t.typeArgs.map(pp(_)).mkString(".")}() : ${pp(t)}"
    }
  }

  private def pp(x: Location, ctxt : ExprContext): String = {
    val suffix = if (ctxt.inOld) { ctxt.oldSuffix } else { "" }
    val prefix = if (ctxt.inOld) { ctxt.oldPrefix } else { "" }
    x match {
      case x@VarAccess(name) if (ctxt.sinkContext.method != None && ctxt.sinkContext.method.get.spec.vars.contains(x.decl)) => s"${prefix}$name${suffix}"
      case x@VarAccess(name) => s"$name"
      case x@FieldAccess(l, name) => {
        val c = l.t.asInstanceOf[ClassType]
        s"${prefix}${c.name}.${name}${suffix}[${pp(l, ctxt)}]"
      }
      case ArrayAccess(l, index)  => {
        s"${prefix}${pp(l.t)}._elems${suffix}[${pp(l, ctxt)}][${pp(index, ctxt)}]"
      }
    }
  }

  private def pp(x: PrimitiveFunction, ctxt : ExprContext): String = {
    val suffix = if (ctxt.inOld) { ctxt.oldSuffix } else { "" }
    val prefix = if (ctxt.inOld) { ctxt.oldPrefix } else { "" }
    x match {
      case Length(expr)         => {
        val c = expr.t.asInstanceOf[ArrayType]
        s"${prefix}${pp(c)}._length${suffix}[${pp(expr, ctxt)}]"
      }
      case Lock(expr)         => {
        val ty = pp(expr.t)
        s"${prefix}${ty}._lock${suffix}[${pp(expr, ctxt)}]"
      }
      case IsLocal(expr, t)     => {
        s"isLocal(${prefix}${pp(expr.t)}._state${suffix}[${pp(expr, ctxt)}], ${pp(t, ctxt)})"
      }
      case IsShared(expr)       => {
        s"isShared(${prefix}${pp(expr.t)}._state${suffix}[${pp(expr, ctxt)}])"
      }
      case IsFresh(expr)       => {
        methodSpecCheckOrElse {
          s"isFresh(${methodSpecPrefix}${pp(expr.t)}._state[${pp(expr, ctxt)}])"
        } {
          "true"
        }
      }
      case NextSpecStep(n) => {
        methodSpecCheckOrElse {
          val method = ctxt.sinkContext.method.get
          method.spec.stateForNextStep(n)
        } {
          "true"
        }
      }

      case Holds(expr, t)       => {
        val v = pp(expr, ctxt)
        val ty = pp(expr.t)
        s"(isAccessible(${prefix}${ty}._state${suffix}[${v}], ${pp(t, ctxt)}) && ${ty}._lock${suffix}[${v}] == ${pp(t)})"
      }
      case MoverPermission(expr,v) => {
        expr match {
          case VarAccess(_) => {
            fail("SinkPrinter", "can't have permission of var")
          }
          case x@FieldAccess(l, name)  => {
            val eval = v match {
              case Some(value) => genWriteEvalCall(x.decl, pp(l),  "tid", pp(value),false)
              case None        => genReadEvalCall(x.decl, pp(l), "tid", false)
            }
            s"${moverOnly(eval)}"
          }
          case x@ArrayAccess(l, index) => {
            val eval = v match {
              case Some(value) => genWriteEvalCall(x.l.t.asInstanceOf[ArrayType].decl, pp(x.l.t.asInstanceOf[ArrayType].thisAccess), pp(l), pp(value), "tid", pp(index), false)
              case None => genReadEvalCall(x.l.t.asInstanceOf[ArrayType].decl, pp(x.l.t.asInstanceOf[ArrayType].thisAccess), pp(l), "tid", pp(index), false)
            }
            s"${moverOnly(eval)}"
          }
          case _ =>
            fail("SinkPrinter", s"can't have permission of ${expr}")
        }
      }
      case GoesWrong(expr) => {
        reductionCheckOrElse {
          s"transition(_pc, ${pp(expr)}) == PhaseError"
        } {
          "false"
        }
      }

      case Rand()               => "*"
    }
  }

  private def pp(x: BinaryOp): String = {
    x match {
      case Add()     => "+"
      case Sub()     => "-"
      case Mul()     => "*"
      case Div()     => " div "
      case Mod()     => " mod "
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

  private def pp(x: UnaryOp): String = {
    x match {
      case Neg() => "-"
      case Not() => "!"
      case Paren() => ""
      case _     => assert(false); ""
    }
  }

  def pp(x: Mover): String = {
    x match {
      case I() => "_I"
      case B() => "_B"
      case N() => "_N"
      case E() => "_E"
      case R() => "_R"
      case L() => "_L"
    }
  }

  def pp(x: Expr, ctxt : ExprContext = ExprContext(new SinkStmtContext(), false, "", "")): String = {
    x match {
      case ConstExpr(c)             => pp(c)
      case BinaryExpr(lhs, rhs, Mod()) if !config.modAxioms => s"MOD(${pp(lhs, ctxt)}, ${pp(rhs, ctxt)})"
      case BinaryExpr(lhs, rhs, op) => s"(${pp(lhs, ctxt)}${pp(op)}${pp(rhs, ctxt)})"
      case UnaryExpr(expr, op)      => s"${pp(op)}(${pp(expr, ctxt)})"
      case Cond(p, tt, ff) => s"(if (${pp(p, ctxt)}) then ${pp(tt, ctxt)} else ${pp(ff, ctxt)})"
      case Quantified(quantifier, decls, pred, triggers)                                               => {
        val ts = triggers.map(_.map(pp(_, ctxt)).mkString("{ ", ", ", " }")).mkString(" ")
        val extras = List() // decls.filter(_.t == IntType()).map(d => s"_trigger(${d.name})")
        val clauses = if (extras.size > 0) {
          extras.mkString("", "&&", "&&")
        } else {
          ""
        }
        val q = quantifier match {
          case ForAll() => "forall"
          case Exists() => "exists"
        }
        s"(${q} ${decls.map(d => s"${d.name}: ${pp(d.t)}").mkString(",")} :: ${ts} ${clauses}(${pp(pred, ctxt)}))"
      }

      case x: PrimitiveFunction     => pp(x, ctxt)
      case x: Location              => pp(x, ctxt)
      case LoweredExpr(e, original)    => s"${pp(e, ctxt)} /* lowered ${pp(original)} */"
      case Old(e) => pp(e, ExprContext(ctxt.sinkContext, true, ctxt.oldPrefix, ctxt.oldSuffix))
      case BuiltInFunctionCall(name, Nil, args) => s"${name}(${args.map(pp(_,ctxt)).mkString(",")})"
      case BuiltInFunctionCall(name, types, args) => s"${name}.${types.map(pp(_)).mkString(".")}(${args.map(pp(_,ctxt)).mkString(",")})"

    }
  }

  def pp(x: Type): String = {
    x match {
      case IntType()                              => "int"
      case BoolType()                             => "bool"
      case VoidType()                             => "void"
      case ClassType(x)                           => x
      case TidType()                              => "Tid"
      case MoverType()                            => "Mover"
      case BoogieType(name)                       => name
      case x@ArrayType(enclosing, ident, thisVar) => s"Array.${x.decl.parent.name}.${ident}"
      case TypeVar(x)                             => x
      case CollectionType(name, Nil)         => name
      case CollectionType(name, typeArgs)         => s"${name}.${typeArgs.map(pp(_)).mkString(".")}"

    }
  }

  def z3pp(x: Type): String = {
    x match {
      case IntType()                              => "Int"
      case BoolType()                             => "Bool"
      case VoidType()                             => ???
      case ClassType(x)                           => s"T@${x}"
      case TidType()                              => "Int"
      case MoverType()                            => "T@Mover"
      case BoogieType(name)                       => name
      case x@ArrayType(enclosing, ident, thisVar) => s"(Array Int ${z3pp(x.elemType())})"
      case TypeVar(x)                             => x
      case CollectionType("Seq", List(t))         => s"(Seq ${z3pp(t)})"
      case CollectionType("Set", List(t))         => s"(Set ${z3pp(t)} Bool)"
      case CollectionType(name, Nil)              => name
      case CollectionType(name, typeArgs)         => s"${name}.${typeArgs.map(z3pp(_)).mkString(".")}"

    }
  }


  def yields(s : Stmt) : Boolean = {
    s match {
      case Block(label, body)                            => body.exists(yields(_))
      case If(cond, trueBranch, falseBranch)             => yields(trueBranch) || yields(falseBranch)
      case While(cond, stmt, invariants, decreases)      => yields(stmt)
      case Yield(ensures)                                => true
      case Sync(lock, stmt, releasePos)                  => yields(stmt)
      case _ => false
    }
  }
}