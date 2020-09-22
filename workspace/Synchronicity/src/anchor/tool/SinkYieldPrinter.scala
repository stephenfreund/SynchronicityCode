package anchor.tool

import anchor.boogie.AnnotatedBuffer
import anchor.sink.AST.pos
import anchor.sink._
import anchor.transforms.DeepCopyWithPositions
import anchor.util.Errors
import anchor.util.Errors.fail

import scala.util.parsing.input.NoPosition
import InvariantPrefix.InvariantPrefix


class SinkYieldPrinter(buffer: AnnotatedBuffer,
                       val config: Config,
                       sprinter : SinkPrinter,
                       program: Program) {



  private def genYieldProc() = {

    buffer.emit(s"procedure Yield(tid: Tid);")
    buffer.emit(s"requires ${sprinter.genStateInvariantCall()};")
    buffer.emit(s"requires ValidTid(tid);")
    sprinter.genGlobalInvariants(buffer, new SinkStmtContext(), InvariantPrefix.requires, program)
    buffer.emit(sprinter.genModifiesList())
    buffer.emit(s"ensures ${sprinter.genStateInvariantCall()};")
    buffer.emit(s"ensures Y(tid ${argSet("old(", ")")} ${argSet()});")
    sprinter.genGlobalInvariants(buffer, new SinkStmtContext(), InvariantPrefix.ensures, program)  // ????

    buffer.emit()
    for (c <- program.classes) {
      for (f <- c.fields) {
        genY_x(buffer, f)
      }
      for (a <- c.arrays) {
        genY_x(buffer, a)
      }
    }

    buffer.emit()
    genY(buffer)

    buffer.emit()
    buffer.emit()

    if (config.inferInvsPreservedByY) {
      for (c <- program.classes; inv <- c.invariants) {
        genInvariantIsPreservedByYieldSteps(buffer, c, inv)
      }
    }
  }

  import sprinter._

  private def genInvariantIsPreservedByYieldSteps(buffer : AnnotatedBuffer, c : ClassDecl, inv : ClassInvariant): Unit  = {
    val (preDecls, preCode) = sprinter.preserveState(c.scope, "_pre", "")
    val (postDecls, postCode) = sprinter.preserveState(c.scope, "_post", "_p")


    buffer.emit(
      s"""
         | const Y.preserves.Invariant.${inv.id} : bool;
         | axiom Y.preserves.Invariant.${inv.id} <==>
         |     (forall tid : Tid, this: ${c.name} ${paramSet()} ${paramSet("", "_p")} ::
         |        ${sprinter.genStateInvariantCall()} &&
         |        ${sprinter.genStateInvariantCall("", "_p")} &&
         |        ValidTid(tid) &&
         | """.stripMargin)
    for (c2 <- program.classes; inv2 <- c2.invariants) {
      buffer.emit(s"""       ${sprinter.genGlobalInv(c2, inv2, "_this", "true", "", "_p")} &&""");
    }
    buffer.emit(
      s"""
         |        Y(tid ${argSet()} ${argSet("", "_p")})
         | ==>
         |        ${sprinter.genGlobalInv(c, inv, "_this", "true", "", "_p")});
         |""".stripMargin)

  }

  private def genPossibleInvariantsAtIntermediateYieldSteps(): List[String]  = {
    for (c <- program.classes; inv <- c.invariants) yield {
      val x = s"(Y.preserves.Invariant.${inv.id} ==> ${sprinter.genGlobalInv(c, inv, "_this", "true")})"
      println(x)
      x
    }
  }

  private def genY(buffer: AnnotatedBuffer) = {
    val fs = for (c <- program.classes; f <- c.fields)
      yield s"(forall this: ${f.parent.name} :: Y_${f.parent.name}.${f.name}(tid : Tid, this, ${c.name}.${f.name}_p[this] ${argSet()}))"


    val as = for (c <- program.classes;
                  a <- c.arrays;
                  name = s"Array.${a.parent.name}.${a.name}")
      yield s"(forall athis: ${name}, index: int :: Y_${name}(tid : Tid, athis, index, ${name}._elems_p[athis][index] ${argSet()}))"


    val states = for (c <- program.classes) yield {
      val name = c.name
      List(s"(forall _i : ${name} :: isShared(${name}._state[_i]) ==> isShared(${name}._state_p[_i]))",
        s"(forall _i : ${name} :: isLocal(${name}._state[_i], tid) <==> isLocal(${name}._state_p[_i], tid))")
    }

    val astates = for (c <- program.classes; a <- c.arrays) yield {
      val name = s"Array.${a.parent.name}.${a.name}"
      List(
        s"(forall _i : ${name} :: isShared(${name}._state[_i]) ==> isShared(${name}._state_p[_i]))",
        s"(forall _i : ${name} :: ${name}._length[_i] == ${name}._length_p[_i])",
        s"(forall _i : ${name} :: isLocal(${name}._state[_i], tid) <==> isLocal(${name}._state_p[_i], tid))")
    }

    val terms = fs ++ as ++ states.flatten ++ astates.flatten

    buffer.emit(
      s"""
         | function {:inline} Y(tid : Tid ${paramSet()} ${paramSet("", "_p")}): bool
         | {
         |@  ${terms.mkString("", "\n@ && ", "\n")}
         | }
         |""".stripMargin)

  }

  private def genY_x(buffer: AnnotatedBuffer, f: FieldDecl) = {
    val c: ClassDecl = f.parent
    buffer.emit(s"// ${c.name}.${f.name}:")
    val triggers = triggersAsString(f.spec, Set("this"))

    val read =  moverOnly(genReadEvalCall(f, "this", "tid", false))
    val write = moverOnly(genWriteEvalCall(f, "this", "u", "newValue", false))

    val ys = s"((isAccessible(${c.name}._state[this], tid) && leq(${read}, _R)) ==> (${c.name}.${f.name}[this] == newValue))" ::
      f.spec.yieldsAs.map(y => s"(${pp(y)})")

    buffer.emit(
      s"""
         | function {:inline} Y_${f.parent.name}.${f.name}(tid : Tid, this: ${f.parent.name}, newValue: ${pp(f.t)} ${paramSet()}): bool
         | {
         |@  ${ys.mkString("", "\n@ &&", "\n")}
         | }
         |""".stripMargin)

    if (sprinter.options.checkSpec) {
      val scope = f.scope

      val parameters =
        List(VarDecl(TidType(), "u"), VarDecl(f.t, "newValue"))

      val (decls, code) = sprinter.preserveState(scope, "_yield", "")

      buffer.emit(s"""
                     | procedure Y_${f.parent.name}.${f.name}.Subsumes.W(tid : Tid, u : Tid, this: ${f.parent.name}, newValue: ${pp(f.t)} ${paramSet()})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ValidTid(tid);
                     | @   requires ValidTid(u) && u != tid;""".stripMargin)
      if (config.inferInvsPreservedByY) {
        for (r <- genPossibleInvariantsAtIntermediateYieldSteps()) {
          buffer.emit(s"   requires ${r};")
        }
      }
      buffer.emit(s"""
                     | {
                     | ${sprinter.genLocals(decls)}
                     | @ assume isAccessible(${f.parent.name}._state[this], tid);
                     | @ assume isAccessible(${f.parent.name}._state[this], u);
                     | @ assume !isError(${write});
                     | @ ${if (f.spec.blocking) { s"assume leq($read, _N);" } else "" }
                     | ${code}
                     | @ assert Y_${f.parent.name}.${f.name}(tid, this, newValue ${argSet()});
                     | }
                     |""".stripMargin, new YieldingError(f,s"yields_as clause for ${f.parent.name}.${f.name} is not valid"))

      buffer.emit(s"""
                     | procedure Y_${f.parent.name}.${f.name}.Reflexive(tid : Tid, this: ${f.parent.name} ${paramSet()})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ValidTid(tid);""".stripMargin)
                    if (config.inferInvsPreservedByY) {
                      for (r <- genPossibleInvariantsAtIntermediateYieldSteps()) {
                        buffer.emit(s"   requires ${r};")
                      }
                    }
                    buffer.emit(s"""
                     | {
                     | ${sprinter.genLocals(decls)}
                     | @ assume isAccessible(${f.parent.name}._state[this], tid);
                     | ${code}
                     | @ assert Y_${f.parent.name}.${f.name}(tid, this, ${f.parent.name}.${f.name}[this] ${argSet()});
                     | }
                     |
           |""".stripMargin, new YieldingError(f,s"yields_as clause for ${f.parent.name}.${f.name} is not reflexive"))

      val (preDecls, preCode) = sprinter.preserveState(scope.pushLocals(List(VarDecl(f.t, "newValue"))), "_pre", "")
      val (postDecls, postCode) = sprinter.preserveState(scope.pushLocals(List(VarDecl(f.t, "newValue"))), "_post", "_p")
      buffer.emit(s"""
                     | procedure Y_${f.parent.name}.${f.name}.Transitive(tid : Tid, this: ${f.parent.name}, newValue : ${pp(f.t)} ${paramSet()} ${paramSet("", "_p")})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ${sprinter.genStateInvariantCall("", "_p")};
                     | @   requires ValidTid(tid);""".stripMargin)
      sprinter.genGlobalInvariants(buffer, new SinkStmtContext(), InvariantPrefix.requires, f.spec)
      buffer.emit(s"""
                     | {
                     | ${sprinter.genLocals(preDecls)}
                     | ${sprinter.genLocals(postDecls)}
                     | ${preCode}
                     | @ assume isAccessible(${f.parent.name}._state[this], tid);
                     | @ assume Y(tid ${argSet()} ${argSet("", "_p")});
                     | @ assume Y_${f.parent.name}.${f.name}(tid, this, newValue ${argSet("", "_p")});
                     | ${postCode}
                     | @ assert Y_${f.parent.name}.${f.name}(tid, this, newValue ${argSet()});
                     | }
                     |
           |""".stripMargin, new YieldingTransitivityError(f,s"yields_as clause for ${f.parent.name}.${f.name} is not transitive"))

      if (f.isABAFree) {
        val (preDecls, preCode) = sprinter.preserveState(scope.pushLocals(List(VarDecl(f.t, "newValue"), VarDecl(f.t, "A"))), "_pre", "")
        val (postDecls, postCode) = sprinter.preserveState(scope.pushLocals(List(VarDecl(f.t, "newValue"), VarDecl(f.t, "A"))), "_post", "")
        buffer.emit(s"""
                       | procedure Check_${f.parent.name}.${f.name}.ABAFree(tid : Tid, this: ${f.parent.name}, A : ${pp(f.t)}, newValue : ${pp(f.t)})
                       | @   modifies ${c.name}.${f.name};
                       | @   requires ${sprinter.genStateInvariantCall()};
                       | @   requires ${c.name}.${f.name}[this] == A;
                       | @   requires isAccessible(${f.parent.name}._state[this], tid);
                       | @   requires ValidTid(tid);""".stripMargin)
        sprinter.genGlobalInvariants(buffer, new SinkStmtContext(), InvariantPrefix.requires, f.spec)
        buffer.emit(s"""
                       | {
                       | ${sprinter.genLocals(preDecls)}
                       | ${sprinter.genLocals(postDecls)}
                       | ${preCode}
                       | @ assume Y_${f.parent.name}.${f.name}(tid, this, newValue ${argSet()});
                       | @ assume A != newValue;
                       | ${c.name}.${f.name}[this] := newValue;
                       | ${postCode}
                       | @ assert !(Y_${f.parent.name}.${f.name}(tid, this, A ${argSet()}));
                       | }
                       |
           |""".stripMargin, new YieldingTransitivityError(f,s"${f.parent.name}.${f.name} is not ABA-free"))
      }

    }
  }



  private def genY_x(buffer: AnnotatedBuffer, a: ArrayDecl) = {
    val enclosing = a.parent
    val name = s"Array.${a.parent.name}.${a.name}"
    val index = a.elemName
    val thisName = s"${name}._this[athis]"
    buffer.emit(s"// $name:")
    val read = sprinter.moverOnly(sprinter.genReadEvalCall(a, thisName, "athis", "tid", index, false))

    val triggers = triggersAsString(a.spec, Set("athis"))

    val ys = s"((isAccessible(${name}._state[athis], tid) && leq(${read}, _R)) ==> (${name}._elems[athis][${index}] == newValue))" ::
      a.spec.yieldsAs.map(y => s"(${pp(y)})")

    buffer.emit(
      s"""
         | function {:inline} Y_${name}(tid : Tid, athis: ${name}, index: int, newValue: ${pp(a.elemType)} ${paramSet()}): bool
         | {
         |  (var this := ${name}._this[athis];
         |@  ${ys.mkString("", "\n@ &&", "\n")}
         |  )
         | }
         |""".stripMargin)

    if (sprinter.options.checkSpec) {
      val write = moverOnly(genWriteEvalCall(a, thisName, "athis", "u", index, "newValue", false))
      val scope = a.spec.scope
      val (decls, code) = sprinter.preserveState(scope, "_yield", "")

      buffer.emit(s"""
                     | procedure Y_${name}.Subsumes.W(tid : Tid, u : Tid, this: ${a.parent.name}, athis: ${name}, index: int, newValue: ${pp(a.elemType)} ${paramSet()})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ValidTid(tid);
                     | @   requires ValidTid(u) && u != tid;
                     | @   requires this == ${name}._this[athis];
                     | {
                     | ${sprinter.genLocals(decls)}
                     | @ assume isAccessible(${name}._state[athis], u);
                     | @ assume !isError(${write});
                     | @ ${if (a.spec.blocking) { s"assume leq($read, _N);" } else "" }
                     | ${code}
                     | @ assert Y_${name}(tid, athis, index, newValue ${argSet()});
                     | }
                     |
           |""".stripMargin, new YieldingError(a, s"yields_as clause for ${name} is not valid"))

      buffer.emit(s"""
                     | procedure Y_${name}.Reflexive(tid : Tid, this: ${a.parent.name}, athis: ${name}, index: int ${paramSet()})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ValidTid(tid);
                     | @   requires this == ${name}._this[athis];
                     | {
                     | ${sprinter.genLocals(decls)}
                     | @ assume isAccessible(${name}._state[athis], tid);
                     | ${code}
                     | @ assert Y_${name}(tid, athis, index, ${name}._elems[athis][index] ${argSet()});
                     | }
                     |
           |""".stripMargin, new YieldingError(a,s"yields_as clause for ${name} is not reflexive"))

      val (preDecls, preCode) = sprinter.preserveState(scope, "_pre", "")
      val (postDecls, postCode) = sprinter.preserveState(scope, "_post", "_p")
      buffer.emit(s"""
                     | procedure Y_${name}.Transitive(tid : Tid, this: ${a.parent.name}, athis: ${name}, index: int, newValue : ${pp(a.elemType)} ${paramSet()} ${paramSet("", "_p")})
                     | @   requires ${sprinter.genStateInvariantCall()};
                     | @   requires ${sprinter.genStateInvariantCall("", "_p")};""".stripMargin)
      sprinter.genGlobalInvariants(buffer, new SinkStmtContext(), InvariantPrefix.requires, a.spec)
      buffer.emit(s"""
                     | @   requires this == ${name}._this[athis];
                     | @   requires ValidTid(tid);
                     | {
                     | ${sprinter.genLocals(preDecls)}
                     | ${sprinter.genLocals(postDecls)}
                     | ${preCode}
                     | ${postCode}
                     | @ assume isAccessible(${name}._state[athis], tid);
                     | @ assume Y(tid ${argSet()} ${argSet("", "_p")});
                     | @ assume Y_${name}(tid, athis, index, newValue ${argSet("", "_p")});
                     | @ assert Y_${name}(tid, athis, index, newValue ${argSet()});
                     | }
                     |
           |""".stripMargin, new YieldingTransitivityError(a,s"yields_as clause for ${name} is not transitive"))
    }
  }



  def argSet(prefix1: String = "", suffix1: String = "") : String = {
    (sprinter.genHeapArgList(prefix1, suffix1)).mkString(", ", ", ", "")
  }

  def paramSet(prefix1: String = "", suffix1: String = "") : String = {
    (sprinter.genHeapParameterList(prefix1, suffix1)).mkString(", ", ", ", "")
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
        case Quantified(_, decls, pred, triggers)         => containsVar(pred) // ignore triggers?
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
            case MoverPermission(loc, v) => containsVar(loc) || v.exists(containsVar((_)))
            case GoesWrong(expr) => containsVar(expr)
            case Rand()                    => false
            case NextSpecStep(step) => false
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
            case _ => Set()
          }
        }
        case function: PrimitiveFunction =>
          function match {
            case IsLocal(expr, t) if containsVar(expr)                           => {
              Set(s"${pp(expr.t)}._state[${pp(expr)}]") ++ etriggers(expr)
            }
            case Holds(expr, t) if containsVar(expr)                             => {
              Set(s"${pp(expr.t)}._lock[${pp(expr)}]") ++ etriggers(expr)
              //fail("SinkPrinter", "Should not get holds here", x)
            }
            case MoverPermission(l, v) if containsVar(l) || v.exists(containsVar(_)) => {
              etriggers(l) ++ (v match {
                case None => Set[String]()
                case Some(v) => etriggers(v)
              })
            }
            case GoesWrong(expr) if containsVar(expr) => {
              etriggers(expr)
            }
            case _ => Set()
          }
        case LoweredExpr(e, original) => etriggers(e)
        case Old(l) => etriggers(l)
        case BuiltInFunctionCall(name, types, args) => args.flatMap(etriggers(_)).toSet

      }
    }
    etriggers(x.conditionalMover)
  }


  def gen() = {
    Errors.check("Yield", sprinter.genHeapParameterList().size > 0, "Yield only works if at least one field.", NoNode(NoPosition))
    genYieldProc()
  }

}