package anchor.tool

import acme.scala.Util
import anchor.boogie.AnnotatedBuffer
import anchor.sink._
import anchor.transforms.Rename

abstract class MemoryAccess(val node: HasSpec, val varName: String) {
  // should be boogie map name...
  def id: String
  def sourceDescription: String

  def refType : Type

  def parameters: List[VarDecl]
  def locType: Type
  def requires(t: String): String

  def genReadEvalCall(sprinter: SinkPrinter, tid: String): String
  def genWriteEvalCall(sprinter: SinkPrinter, tid: String, newValueName: String): String

  def spec: Spec

  def boogieAccess: String
  def sinkAccess: String
}

class FieldMemoryAccess(val fd: FieldDecl,
                        override val varName: String) extends MemoryAccess(fd, varName) {

  def id = s"${fd.parent.name}.${fd.name}"
  def sourceDescription = id

  def refType: Type = ClassType(fd.parent.name)

  def parameters = List(VarDecl(refType, varName)) // NOTE: not typechecked!

  def locType = fd.t


  def requires(tid: String) = s"requires isAccessible(${fd.parent.name}._state[${varName}], ${tid});"

  def genReadEvalCall(sprinter: SinkPrinter, tid: String) = {
    sprinter.genReadEvalCall(fd, varName, tid, false)
  }

  def genWriteEvalCall(sprinter: SinkPrinter, tid: String, newValueName: String) = {
    sprinter.genWriteEvalCall(fd, varName, tid, newValueName, false)
  }

  def boogieAccess = s"${fd.parent.name}.${fd.name}[${varName}]"
  def sinkAccess = s"${varName}.${fd.name}"
  def spec : Spec = {
    val s = Rename(node.spec, Map( (node.scope.resolveVar("this").get -> VarDecl(ClassType(fd.parent), varName))))
    BuildScope.annotate(node.spec.scope.renameVar("this", varName), s, fd.t)
    s
  }
}

class ArrayMemoryAccess(val ad: ArrayDecl,
                        override val varName: String,
                        val index: String) extends MemoryAccess(ad, varName) {
  val refType = ArrayType(ad.parent.name, ad.name, VarAccess(s"${varName}_owner"))
  refType.decl = ad

  private val arrayStr = s"Array.${ad.parent.name}.${ad.name}"

  def id = s"${arrayStr}._elems"

  def sourceDescription = s"Array ${arrayStr}"

  def parameters = List(
    VarDecl(ClassType(ad.parent.name), varName + "_owner"),
    VarDecl(refType, varName),
    VarDecl(IntType(), index)
  )
//    s"${varName}_owner: ${ad.parent.name}, ${varName}: ${arrayStr}, ${index}: int"

  def locType = refType.elemType

  def requires(tid: String) =
    s"""requires isAccessible(${arrayStr}._state[${varName}], ${tid});
       |requires ${arrayStr}._this[${varName}] == ${varName}_owner;""".stripMargin

  def genReadEvalCall(sprinter: SinkPrinter, tid: String) = {
    sprinter.genReadEvalCall(ad, s"${varName}_owner", varName, tid, index, false)
  }

  def genWriteEvalCall(sprinter: SinkPrinter, tid: String, newValueName: String) = {
    sprinter.genWriteEvalCall(ad, s"${varName}_owner", varName, tid, index, newValueName, false)
  }


  def boogieAccess = s"${arrayStr}._elems[${varName}][${index}]"

  def sinkAccess = s"${varName}[${index}]"

  def spec : Spec = {
    val scope = node.spec.scope
    val renamedScope = scope.renameVar("athis", varName).renameVar("this", s"${varName}_owner").renameVar("index", index)

    val s = Rename(node.spec,
      Map((scope.resolveVar("athis").get -> VarDecl(refType, varName)),
        (scope.resolveVar("this").get -> VarDecl(ClassType(ad.parent), s"${varName}_owner")),
        (scope.resolveVar("index").get -> VarDecl(IntType(), index))))
    BuildScope.annotate(renamedScope, s, ad.elemType)
    s
  }

}

sealed abstract class Access {
  def short: String
  def tid: String
  def spec: Spec
  def evalPrefix : String

  def decls() = {
    s"""var ${evalPrefix} : MoverPath;
       |var ${evalPrefix}_Mover : Mover;
       |var ${evalPrefix}_Path : int;""".stripMargin
  }

  def eval(sprinter : SinkPrinter): String
}

case class ReadAccess(a : MemoryAccess, tid: String, evalPrefix : String) extends Access {
  def short = "Read"
  def spec : Spec = {
    val node = a.node
    val s = Rename(a.spec, a.spec.scope.resolveVar("tid").get, VarDecl(TidType(), tid))
    BuildScope.annotate(a.spec.scope.renameVar("tid", tid), s, a.locType)
    s
  }
  def eval(sprinter : SinkPrinter): String = {
    s"""${evalPrefix} := ${a.genReadEvalCall(sprinter, tid)};
    ${evalPrefix}_Mover := m#moverPath(${evalPrefix});
    ${evalPrefix}_Path := p#moverPath(${evalPrefix});"""
  }
}

case class WriteAccess(a : MemoryAccess, tid: String, value: String, evalPrefix : String) extends Access {
  def short = "Write"
  def spec: Spec = {
    val node = a.node
    val s = Rename(a.spec, a.spec.scope.resolveVar("tid").get, VarDecl(TidType(), tid))
    BuildScope.annotate(a.spec.scope.renameVar("tid", tid), s, a.locType)
    s
  }
  def eval(sprinter : SinkPrinter): String = {
    s"""${evalPrefix} := ${a.genWriteEvalCall(sprinter, tid, value)};
    ${evalPrefix}_Mover := m#moverPath(${evalPrefix});
    ${evalPrefix}_Path := p#moverPath(${evalPrefix});"""
  }
}

case class ValidityError(val a: MemoryAccess, check : String, val x: Access, val y: Access)
  extends SinkError(a.node, s"${a.sourceDescription} failed ${x.short}-${y.short} ${check}") {

  private def eval(acc: Access, model: SinkModel) = PrettyPrint.pp(acc.spec, model.localInt(s"${acc.evalPrefix}_Path@0").get)

  def path(acc : Access, ctxt: SinkErrorContext): Int = {
    ctxt.model.get.localInt(s"${acc.evalPrefix}_Path@0").get
  }

  override def toLongString(ctxt: SinkErrorContext): String = {
    if (ctxt.model == None) {
      this.toLongString()
    } else {
      val model = ctxt.model.get

      def accessToString(acc: Access) = {
        val step = acc match {
          case ReadAccess(a, tid, evalPath)         =>
            f"""    Thread ${acc.tid}%-5s: _ := ${a.sinkAccess};"""
          case WriteAccess(a, tid, value, evalPath) =>
            f"""    Thread ${acc.tid}%-5s: ${a.sinkAccess} := ${model.local(value).getOrElse("_")};"""
        }
        step + s"""  [${PrettyPrint.pp(acc.spec, model.localInt(s"${acc.evalPrefix}_Path@0").get)}]"""
      }
      super.toLongString(ctxt) + "\n" +
        accessToString(x) + "\n" +
        accessToString(y) + "\n"
    }
  }

  override def toHTML(ctxt: SinkErrorContext) : String = {
    this.toLongString(ctxt) + "\n    " + String.format(s"""<a target="heap_frame" href="./%s._pre.pdf">Heap</a>""", s"${id}")
  }

}


case class OldStabilityError(val a: MemoryAccess, check : String, detail: String = "", val origFirst: Access, val origSecond: Access, val flippedFirst: Access, val flippedSecond: Access)
  extends SinkError(a.node, s"${a.sourceDescription} failed ${check} ${detail}") {

  private def eval(acc: Access, model: SinkModel) = PrettyPrint.pp(acc.spec, model.localInt(s"${acc.evalPrefix}_Path@0").get)

  override def toLongString(ctxt: SinkErrorContext) : String = {
    toLongStringHelper(ctxt, (x:String) => x)
  }

  def toLongStringHelper(ctxt: SinkErrorContext, extraWrite : String => String) : String = {
    if (ctxt.model == None) {
      this.toLongString()
    } else {

      val model = ctxt.model.get

      def accessToString(acc: Access) = {
        val step = acc match {
          case ReadAccess(a, tid, evalPath)         =>
            f"""Thread ${tid}%-5s: _ := ${a.sinkAccess};   [${eval(acc, model)}]"""
          case WriteAccess(a, tid, value, evalPath) =>
            f"""Thread ${tid}%-5s: ${a.sinkAccess} := ${model.local(value).getOrElse("_")};   [${eval(acc, model)}]"""
        }
        step
      }

      val name = this.id + ""

      SpecPrinter.dot(origSecond.spec, Util.fresh("error"), Some(model.localInt(s"${origSecond.evalPrefix}_Path@0").get))
      SpecPrinter.dot(flippedFirst.spec, Util.fresh("error"), Some(model.localInt(s"${flippedFirst.evalPrefix}_Path@0").get))
      super.toLongString(ctxt) + "\n\n" +
        "    " + accessToString(origFirst) + "\n" +
        "    " + extraWrite(accessToString(origSecond)) + "\n\n" +
        "    BUT...\n\n" +
        "    " + extraWrite(accessToString(flippedFirst)) + "\n" +
        "    " + accessToString(flippedSecond) + "\n"
    }
  }

  override def toHTML(ctxt: SinkErrorContext) : String = {
    this.toLongStringHelper(ctxt, (x : String) => String.format(s"""<a target="heap_frame" href="./%s._delta.pdf">%s</a>""", s"${id}", x))
  }
}


abstract class StabilityTerm {
  def eval(model: SinkModel) : String
}
case class StabilityAccess(val a : Access) extends StabilityTerm {
  def eval(model: SinkModel) : String = {
    s"${model.local(s"${a.evalPrefix}_Mover@0").get}"
  }
  override def toString: String = a.evalPrefix + "_Mover"
}
case class StabilityConstant(val a : Mover) extends StabilityTerm {
  def eval(model: SinkModel) : String = {
    PrettyPrint.pp(a)
  }
  override def toString: String = a match {
    case B() => "_B"
    case R() => "_R"
    case L() => "_L"
    case N() => "_N"
    case E() => "_E"
    case I() => "_I"
  }
}

abstract class StabilityConstraint {
  def eval(model: SinkModel) : String
}

case class StabilityNotAlias(val lhs : String, val rhs : String) extends StabilityConstraint {
  def eval(model: SinkModel) : String = {
    return s"${lhs} != ${rhs}"
  }
  override def toString: String = s"${lhs} != ${rhs}"

}

case class StabilityLEQ(val lhs: StabilityTerm, val rhs: StabilityTerm) extends StabilityConstraint {
  def eval(model: SinkModel) : String = {
    return s"${lhs.eval(model)} <= ${rhs.eval(model)}"
  }
  override def toString: String = s"leq(${lhs}, ${rhs})"

  }
case class StabilityNotLEQ(val lhs: StabilityTerm, val rhs: StabilityTerm) extends StabilityConstraint {
  def eval(model: SinkModel) : String = {
    return s"${lhs.eval(model)} > ${rhs.eval(model)}"
  }
  override def toString: String = s"!leq(${lhs}, ${rhs})"
}
case class StabilityIn(val lhs: StabilityTerm, val members: List[StabilityTerm]) extends StabilityConstraint {
  def eval(model: SinkModel): String = {
    return s"${lhs.eval(model)} in { ${members.map(_.eval(model)).mkString(",")} }"
  }
  override def toString: String = members.map(s => s"${lhs} == ${s}").mkString("(", " || ", ")")
}

case class StabilityNone() extends StabilityConstraint {
  def eval(model: SinkModel): String = {
    return "true"
  }

  override def toString: String = "true"
}

case class StabilitySeqItem(val access: Access, val heapSuffix: String, val constraint: StabilityConstraint) {}

case class UpdatedHeap(val name : String, val base : String, val writes : List[WriteAccess])

case class StabilityError(val a: MemoryAccess, val b: MemoryAccess, check : String, detail: String = "",
                          val seq1 : List[StabilitySeqItem],
                          val seq2 : List[StabilitySeqItem],
                          val heaps: List[UpdatedHeap] = List.empty)
  extends SinkError(a.node, s"${a.sourceDescription} is not ${check} with respect to ${b.sourceDescription} (case ${detail})") {

  def gen(buffer : AnnotatedBuffer) = {
    val assumes = seq1.map(_.constraint.toString).mkString("(", " && ", ")")
    val asserts = seq2.map(_.constraint.toString).mkString("(", " && ", ")")

    buffer.emit(s"assert ${assumes} ==> ${asserts};", this)
  }

  private def path(acc: Access, model: SinkModel) =
    PrettyPrint.pp(acc.spec, model.localInt(s"${acc.evalPrefix}_Path@0").get)

  override def toLongString(ctxt: SinkErrorContext) : String = {
    toLongStringHelper(ctxt, (x,y) => x)
  }

  def toLongStringHelper(ctxt: SinkErrorContext, linkWriter : (String, StabilitySeqItem) => String) : String = {
    if (ctxt.model == None) {
      this.toLongString()
    } else {

      val model = ctxt.model.get

      def accessToString(acc: Access) = {
        acc match {
          case ReadAccess(a, tid, evalPath)         =>
            f"""_ := ${a.sinkAccess};"""
          case WriteAccess(a, tid, value, evalPath) =>
            f"""${a.sinkAccess} := ${model.local(value).getOrElse("_")};"""
        }
      }

      def step(as:StabilitySeqItem) = {
        val StabilitySeqItem(a,h,s) = as
        val code = accessToString(a)
        val line = linkWriter(code, as) + (" " * Math.max(0, 20-code.length))
        f"""    Thread ${a.tid}%-1s in ${as.heapSuffix}%5s: ${line}  [${s.eval(model)}%-7s]  // ${path(a, model)}"""
     }

      def heapToString(h : UpdatedHeap) = {
        s"${h.name} == ${h.base}${h.writes.map(w => s"[${accessToString(w)}]").mkString("")}"
      }

      super.toLongString(ctxt) + "\n\n" +
      s"    Other Access (${b.node.pos}):\n\n${b.node.pos.longString}\n" +
        seq1.map(step(_)).mkString("\n", "\n", "\n") +
        "\n    BUT...\n" +
        seq2.map(step(_)).mkString("\n", "\n", "\n") +
        "\n    WHERE \n" +
        heaps.map(x => s"    ${heapToString(x)}").mkString("\n", "\n", "\n")
    }
  }

  override def toHTML(ctxt: SinkErrorContext) : String = {
    this.toLongStringHelper(ctxt, (x, y) => String.format(s"""<a target="heap_frame" href="./%s.%s.pdf">%s</a>""", s"${id}", y.heapSuffix, x))
  }
}


class SpecChecks(buffer: AnnotatedBuffer,
                 sprinter : SinkPrinter) {

  val names = List( ("x", "i"), ("y", "j"))

  val sameValueCommutes = sprinter.config.sameValueWritesCommute

  def toMemoryAccess(h : HasSpec, nameIndex : Int) = {
    val (name, index) = names(nameIndex)
    h match {
      case x: FieldDecl => new FieldMemoryAccess(x, name)
      case x: ArrayDecl => new ArrayMemoryAccess(x, name, index)
    }
  }

  def check(hs : List[HasSpec]) = {

    for (h <- hs) {
      checkMovers(buffer, toMemoryAccess(h, 0))
    }

    for (a <- hs; b <- hs) {
      checkStability(buffer, toMemoryAccess(a, 0), toMemoryAccess(b, 1))
      if (a.isInstanceOf[FieldDecl] &&
          a.asInstanceOf[FieldDecl].hasCASOperation) {
        checkStabilityM(buffer, toMemoryAccess(a, 0), toMemoryAccess(b, 1))
        checkStabilityN(buffer, toMemoryAccess(a, 0), toMemoryAccess(b, 1))
      }
    }

  }

  def validityMethodSpec(a : MemoryAccess) = {
    s"""requires ${sprinter.genStateInvariantCall()};
       |requires ValidTid(t);
       |requires ValidTid(u);
       |requires t != u;
       |${a.requires("t")}
       |${a.requires("u")}
       |modifies ${a.id};
     """.stripMargin
  }

  def checkMovers(buffer: AnnotatedBuffer, a: MemoryAccess) = {
    checkWriteRight(buffer, a)
    checkWriteLeft(buffer, a)
    checkReadRight(buffer, a)
    checkReadLeft(buffer, a)
  }

  def checkWriteRight(buffer: AnnotatedBuffer, a: MemoryAccess) = {
    val writeByT = WriteAccess(a, "t", "v", "_writeByT")
    val writeByU = WriteAccess(a, "u", "w", "_writeByU")
    val readByU = ReadAccess(a, "u", "_readByU")
    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(a.locType, "v") :: VarDecl(a.locType, "w") :: a.parameters

    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "",new SinkStmtContext())
    buffer.emit(s"""
                   |procedure _CheckWriteWrite.RightMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${writeByT.decls()}
                   |${writeByU.decls()}
                   |${sprinter.genLocals(preDecls)}
                   |${preCode}
                   |${writeByT.eval(sprinter)}
                   |assume !isError(_writeByT_Mover);
                   |assume leq(_writeByT_Mover,_R);
                   |${if (sameValueCommutes) s"assume ${writeByU.value} != ${writeByT.value};" else ""}
                   |${a.boogieAccess} := ${writeByT.value};
                   |// Do we need to share writeByT.value if it is local?
                   |${writeByU.eval(sprinter)}""".stripMargin)
    buffer.emit("assert isError(_writeByU_Mover);", new ValidityError(a, "Right-Mover Check", writeByT, writeByU))
    buffer.emit("}")
    buffer.emit("")
    buffer.emit(s"""
                   |procedure _CheckWriteRead.RightMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${writeByT.decls()}
                   |${readByU.decls()}
                   |${sprinter.genLocals(preDecls)}
                   |${preCode}
                   |${writeByT.eval(sprinter)}
                   |assume !isError(_writeByT_Mover);
                   |assume leq(_writeByT_Mover,_R);
                   |${if (sameValueCommutes) s"assume ${a.boogieAccess} != ${writeByT.value};" else ""}
                   |${a.boogieAccess} := ${writeByT.value};
                   |// Do we need to share writeByT.value if it is local?""".stripMargin)
    buffer.emit(s"${readByU.eval(sprinter)}")
    buffer.emit("assert _readByU_Mover == _E;", new ValidityError(a, "Right-Mover Check", writeByT, readByU))
    buffer.emit("}")
    buffer.emit("")
  }

  def checkWriteLeft(buffer: AnnotatedBuffer, a: MemoryAccess) = {
    val writeByT = WriteAccess(a, "t", "v", "_writeByT")
    val writeByU = WriteAccess(a, "u", "w", "_writeByU")
    val readByU = ReadAccess(a, "u", "_readByU")
    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(a.locType, "v") :: VarDecl(a.locType, "w") :: a.parameters
    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())

    buffer.emit(s"""
                   |procedure _CheckWriteWrite.LeftMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${writeByT.decls()}
                   |${writeByU.decls()}
                   |var havocValue : ${sprinter.pp(a.locType)};
                   |${sprinter.genLocals(preDecls)}
                   |${if (sameValueCommutes) s"assume ${writeByU.value} != ${writeByT.value};" else ""}
                   |assume w == ${a.boogieAccess};
                   |${preCode}
                   |${writeByT.eval(sprinter)}
                   |assume !isError(_writeByT_Mover);
                   |assume leq(_writeByT_Mover,_L);
                   """.stripMargin)
    buffer.emit(s"""
                   |${a.boogieAccess} := havocValue;
                   |// Do we need to share writeByT.value if it is local?
                   |${writeByU.eval(sprinter)}""".stripMargin)
    buffer.emit("assert isError(_writeByU_Mover);", new ValidityError(a, "Left-Mover Check", writeByT, writeByU))
    buffer.emit("}")
    buffer.emit("")

    buffer.emit(s"""
                   |procedure _CheckWriteRead.LeftMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${writeByT.decls()}
                   |${readByU.decls()}
                   |var havocValue : ${sprinter.pp(a.locType)};
                   |${sprinter.genLocals(preDecls)}
                   |${if (sameValueCommutes) s"assume ${a.boogieAccess} != ${writeByT.value};" else ""}
                   |assume w == ${a.boogieAccess};
                   |${preCode}
                   |${readByU.eval(sprinter)}
                   |${writeByT.eval(sprinter)}
                   |assume !isError(_writeByT_Mover);
                   |assume leq(_writeByT_Mover,_L);
                   """.stripMargin)
    buffer.emit("assert _readByU_Mover == _E;", new ValidityError(a, "Left-Mover Check", writeByT, readByU))
    buffer.emit("}")
    buffer.emit("")
  }

  def checkReadRight(buffer: AnnotatedBuffer, a: MemoryAccess) = {
    val readByT = ReadAccess(a, "t", "_readByT")
    val writeByU = WriteAccess(a, "u", "w", "_writeByU")
    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(a.locType, "v") :: VarDecl(a.locType, "w") :: a.parameters
    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())

    buffer.emit(s"""
                   |procedure _CheckRead.RightMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${readByT.decls()}
                   |${writeByU.decls()}
                   |${sprinter.genLocals(preDecls)}
                   |${if (sameValueCommutes) s"assume ${a.boogieAccess} != ${writeByU.value};" else ""}
                   |${preCode}
                   |${readByT.eval(sprinter)}
                   |${writeByU.eval(sprinter)}
                   |assume leq(_readByT_Mover,_R);
                   """.stripMargin)
    buffer.emit("assert isError(_writeByU_Mover);", new ValidityError(a, "Right-Mover Check", readByT, writeByU))
    buffer.emit("}")
    buffer.emit("")
  }

  def checkReadLeft(buffer: AnnotatedBuffer, a: MemoryAccess) = {
    val readByT = ReadAccess(a, "t", "_readByT")
    val writeByU = WriteAccess(a, "u", "w", "_writeByU")
    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(a.locType, "v") :: VarDecl(a.locType, "w") :: a.parameters
    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())

    buffer.emit(s"""
                   |procedure _CheckRead.LeftMover.${a.id}(${sprinter.genParams(parameters)})
                   |${validityMethodSpec(a)}
                   |{
                   |${readByT.decls()}
                   |${writeByU.decls()}
                   |var havocValue : ${sprinter.pp(a.locType)};
                   |${sprinter.genLocals(preDecls)}
                   |assume w == ${a.boogieAccess};
                   |${if (sameValueCommutes) s"assume ${a.boogieAccess} != ${writeByU.value};" else ""}
                   |${preCode}
                   |${readByT.eval(sprinter)}
                   |${a.boogieAccess} := havocValue;
                   |${writeByU.eval(sprinter)}
                   |assume leq(_readByT_Mover,_L);
                   """.stripMargin)
    buffer.emit("assert isError(_writeByU_Mover);", new ValidityError(a, "Left-Mover Check", readByT, writeByU))
    buffer.emit("}")
    buffer.emit("")
  }

  def stabilityMethodSpec(a : MemoryAccess) = {
    s"""requires ${sprinter.genStateInvariantCall()};
       |requires ValidTid(t);
       |${a.requires("t")}
       |${sprinter.genModifiesList()}
     """.stripMargin
  }

  /////////////////////////////

  def newMethodSpec(a : MemoryAccess, b: MemoryAccess) = {
    s"""requires ${sprinter.genStateInvariantCall()};
       |requires ValidTid(t);
       |requires ValidTid(u);
       |requires t != u;
       |${a.requires("t")}
       |${b.requires("u")}
       |modifies ${a.id};
       |modifies ${b.id};
     """.stripMargin
  }


  def checkStability(buffer: AnnotatedBuffer, l: MemoryAccess, k: MemoryAccess) = {
    val writeByU = WriteAccess(k, "u", "w", "_writeByU")
    val writeByUPost = WriteAccess(k, "u", "w", "_writeByUPost")

    val readByU = ReadAccess(k, "u", "_readByU")
    val readByUPost = ReadAccess(k, "u", "_readByUPost")

    val readByT = ReadAccess(l, "t", "_readByT")
    val readByTPost = ReadAccess(l, "t", "_readByTPost")

    val writeByT = WriteAccess(l, "t", "v", "_writeByT")
    val writeByTPost = WriteAccess(l, "t", "v", "_writeByTPost")

    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(l.locType, "v") :: VarDecl(k.locType, "w") :: VarDecl(k.locType, "w0") :: (l.parameters ++ k.parameters)

    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())
    val (midDecls, midCode) = sprinter.preserveState(parameters, "_mid", "", new SinkStmtContext())
    val (postDecls, postCode) = sprinter.preserveState(parameters, "_post", "", new SinkStmtContext())

    val caseA1 = StabilityError(k,l, "Write-Write Stable", "A.1",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(R()))),
        StabilitySeqItem(writeByU, "_post", StabilityLEQ(StabilityAccess(writeByUPost), StabilityConstant(E())))),
      List(
        StabilitySeqItem(writeByU, "_pre", StabilityIn(StabilityAccess(writeByU), List(StabilityAccess(writeByUPost), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      )
    )

    val caseA2 = StabilityError(k,l, "Write-Write Stable", "A.2",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(writeByUPost, "_post", StabilityNotLEQ(StabilityAccess(writeByUPost), StabilityConstant(L())))),
      List(
        StabilitySeqItem(writeByU, "_pre", StabilityNotLEQ(StabilityAccess(writeByU), StabilityConstant(L())))
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      )
    )

    val caseA3 = StabilityError(k,l, "Write-Write Stable", "A.3",
      List(
        StabilitySeqItem(writeByT, "_pre", if (l.refType != k.refType) StabilityNone() else StabilityNotAlias("x", "y")),
        StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(writeByUPost, "_post", StabilityLEQ(StabilityAccess(writeByUPost), StabilityConstant(L())))),
      List(
        StabilitySeqItem(writeByU, "_pre", StabilityIn(StabilityAccess(writeByUPost), List(StabilityAccess(writeByUPost), StabilityConstant(E()))))
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      )
    )

    buffer.emit(s"""
                   |procedure Stable.Check.A.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | ${writeByU.decls()}
                   | ${writeByT.decls()}
                   | ${writeByUPost.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${writeByU.eval(sprinter)}
                   | ${writeByT.eval(sprinter)}
                   |
                   | ${preCode}
                   | ${l.boogieAccess} := v;
                   | ${postCode}
                   |
                   | ${writeByUPost.eval(sprinter)}
                   | ${if (l.spec.blocking) "assume _writeByT_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (k.spec.blocking) "assume _writeByUPost_Mover != _E;  // will block until it can go..." else ""}
                   |
                   |""".stripMargin)

    caseA1.gen(buffer)
    caseA2.gen(buffer)
    caseA3.gen(buffer)

    buffer.emit(s"""
                   |}
                   |""".stripMargin)

    buffer.emit()
    buffer.emit()
    buffer.emit()

    val caseC = StabilityError(l,k, "Write-Write Stable", "C",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(E()))),
        StabilitySeqItem(writeByU, "_mid", StabilityLEQ(StabilityAccess(writeByU), StabilityConstant(L())))),
      List(
        StabilitySeqItem(writeByTPost, "_post", StabilityIn(StabilityAccess(writeByTPost), List(StabilityAccess(writeByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_mid", "_pre", List(writeByT)),
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )

    buffer.emit(s"""
                   |procedure Stable.Check.C.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | var tmpV : ${sprinter.pp(l.locType)};
                   | ${writeByU.decls()}
                   | ${writeByT.decls()}
                   | ${writeByTPost.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(midDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${preCode}
                   | ${writeByT.eval(sprinter)}
                   |
                   | tmpV := ${l.boogieAccess};
                   | ${l.boogieAccess} := v;
                   |
                   | ${midCode}
                   | ${writeByU.eval(sprinter)}
                   |
                   | ${l.boogieAccess} := tmpV;
                   | ${k.boogieAccess} := w;
                   | ${writeByTPost.eval(sprinter)}
                   | ${postCode}
                   |
                   | ${if (l.spec.blocking) "assume _writeByT_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (l.spec.blocking) "assume _writeByTPost_Mover != _E;  // will block until it can go..." else ""}
                   |
                 |""".stripMargin)
    caseC.gen(buffer)
    buffer.emit(s"""
                   |}
                   |""".stripMargin)

    val caseD = StabilityError(l,k, "Write-Write Stable", "D",
      List(
        StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(R()))),
        StabilitySeqItem(writeByU, "_mid", StabilityLEQ(StabilityAccess(writeByUPost), StabilityConstant(N())))),
      List(
        StabilitySeqItem(writeByUPost, "_post", StabilityIn(StabilityAccess(writeByTPost), List(StabilityAccess(writeByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_mid", "_pre", List(writeByT)),
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )

    val caseE = StabilityError(l,k, "Write-Write Stable", "R",
      List(
        StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(writeByU, "_mid", StabilityLEQ(StabilityAccess(writeByUPost), StabilityConstant(L())))),
      List(
        StabilitySeqItem(writeByUPost, "_post", StabilityIn(StabilityAccess(writeByTPost), List(StabilityAccess(writeByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_mid", "_pre", List(writeByT)),
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )


    buffer.emit(s"""
                   |procedure Stable.Check.DE.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | var tmpV : ${sprinter.pp(l.locType)};
                   | ${writeByU.decls()}
                   | ${writeByT.decls()}
                   | ${writeByUPost.decls()}
                   | ${writeByTPost.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(midDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${writeByU.eval(sprinter)}
                   | ${writeByT.eval(sprinter)}
                   |
                   | ${preCode}
                   | tmpV := ${l.boogieAccess};
                   | ${l.boogieAccess} := v;
                   | ${midCode}
                   |
                   | ${writeByUPost.eval(sprinter)}
                   |
                   | ${l.boogieAccess} := tmpV;
                   | ${k.boogieAccess} := w;
                   | ${writeByTPost.eval(sprinter)}
                   |
                   | ${postCode}
                   |
                   | ${if (l.spec.blocking) "assume _writeByT_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (l.spec.blocking) "assume _writeByTPost_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (k.spec.blocking) "assume _writeByUPost_Mover != _E;  // will block until it can go..." else ""}
                   |
                 |""".stripMargin)
//    buffer.emit(s"""
//                   |assert leq(_writeByU_Mover, _L)
//                   |     ==> eqOrError(_writeByTPost_Mover, _writeByT_Mover);
//                """.stripMargin, new OldStabilityError(l, "Stable Write-Write Check", "C", writeByT, writeByUPost, writeByU, writeByTPost))
//    buffer.emit(s"""
//                   |assert leq(_writeByT_Mover, _R) && leq(_writeByUPost_Mover, _N)
//                   |     ==> eqOrError(_writeByTPost_Mover, _writeByT_Mover);
//                """.stripMargin, new OldStabilityError(l, "Stable Write-Write Check", "D", writeByT, writeByUPost, writeByU, writeByTPost))
//    buffer.emit(s"""
//                   |assert leq(_writeByT_Mover, _N) && leq(_writeByUPost_Mover, _L)
//                   |     ==> eqOrError(_writeByTPost_Mover, _writeByT_Mover);
//                """.stripMargin, new OldStabilityError(l, "Stable Write-Write Check", "E", writeByT, writeByUPost, writeByU, writeByTPost))
//
    caseD.gen(buffer)
    caseE.gen(buffer)

    buffer.emit(s"""
                   |}
                   |""".stripMargin)


    buffer.emit()

    val caseF = StabilityError(l,k, "Read-Write Stable", "F",
      List(
        StabilitySeqItem(readByT, "_pre", StabilityLEQ(StabilityAccess(readByT), StabilityConstant(R()))),
        StabilitySeqItem(writeByU, "_pre", StabilityLEQ(StabilityAccess(writeByU), StabilityConstant(N())))),
      List(
        StabilitySeqItem(readByTPost, "_post", StabilityIn(StabilityAccess(readByTPost), List(StabilityAccess(readByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )

    val caseH = StabilityError(l,k, "Read-Write Stable", "H",
      List(
        StabilitySeqItem(readByT, "_pre", StabilityLEQ(StabilityAccess(readByT), StabilityConstant(E()))),
        StabilitySeqItem(writeByU, "_pre", StabilityLEQ(StabilityAccess(writeByU), StabilityConstant(L())))),
      List(
        StabilitySeqItem(readByTPost, "_post", StabilityIn(StabilityAccess(readByTPost), List(StabilityAccess(readByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )

    val caseI = StabilityError(l,k, "Read-Write Stable", "I",
      List(
        StabilitySeqItem(readByT, "_pre", if (l.refType != k.refType) StabilityNone() else StabilityNotAlias("x", "y")),
        StabilitySeqItem(readByT, "_pre", StabilityLEQ(StabilityAccess(readByT), StabilityConstant(N()))),
        StabilitySeqItem(writeByU, "_pre", StabilityLEQ(StabilityAccess(writeByU), StabilityConstant(N())))),
      List(
        StabilitySeqItem(readByTPost, "_post", StabilityIn(StabilityAccess(readByTPost), List(StabilityAccess(readByT), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByU))
      )
    )


    buffer.emit(s"""
                   |procedure Stable.Check.FHI.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | ${writeByU.decls()}
                   | ${readByT.decls()}
                   | ${readByTPost.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${readByT.eval(sprinter)}
                   | ${writeByU.eval(sprinter)}
                   |
                   | ${preCode}
                   | ${k.boogieAccess} := w;
                   | ${postCode}
                   |
                   | ${readByTPost.eval(sprinter)}
                   | ${if (k.spec.blocking) "assume _writeByU_Mover != _E;  // will block until it can go..." else ""}
                   |
                   |""".stripMargin)
//    buffer.emit(s"""
//                   |assert leq(_readByT_Mover, _R) && leq(_writeByU_Mover, _N)
//                   |     ==> eqOrError(_readByTPost_Mover, _readByT_Mover);
//                  """.stripMargin, new OldStabilityError(l, "Stable Read-Write Check", "F", readByT, writeByU, writeByU, readByTPost))
//    buffer.emit(s"""
//                   |assert leq(_writeByU_Mover, _L)
//                   |     ==> eqOrError(_readByTPost_Mover, _readByT_Mover);
//                 """.stripMargin, new OldStabilityError(l, "Stable Read-Write Check", "H", writeByT, writeByU, writeByU, readByTPost))
//    buffer.emit(s"""
//                   |assert ${if (l.refType != k.refType) "" else "x != y && "}
//                   |       leq(_readByT_Mover, _N) && leq(_writeByU_Mover, _N)
//                   |     ==> eqOrError(_readByTPost_Mover, _readByT_Mover);
//                 """.stripMargin, new OldStabilityError(l, "Stable Read-Write Check", "I", writeByT, writeByU, writeByU, readByTPost))
    caseF.gen(buffer)
    caseH.gen(buffer)
    caseI.gen(buffer)
    buffer.emit(s"""
                   |}
                   |""".stripMargin)

    val caseJ = StabilityError(k,l, "Write-Read Stable", "J",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(R()))),
        StabilitySeqItem(readByUPost, "_post", StabilityLEQ(StabilityAccess(readByUPost), StabilityConstant(E())))),
      List(
        StabilitySeqItem(readByU, "_pre", StabilityIn(StabilityAccess(readByU), List(StabilityAccess(readByUPost), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      ))
    val caseK = StabilityError(k,l, "Write-Read Stable", "K",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(readByUPost, "_post", StabilityLEQ(StabilityAccess(readByUPost), StabilityConstant(L())))),
      List(
        StabilitySeqItem(readByU, "_pre", StabilityIn(StabilityAccess(readByU), List(StabilityAccess(readByUPost), StabilityConstant(E())))),
      ),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      ))
    val caseL = StabilityError(k,l, "Write-Read Stable", "L",
      List( StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(readByUPost, "_post", StabilityNotLEQ(StabilityAccess(readByUPost), StabilityConstant(L())))),
      List(
        StabilitySeqItem(readByU, "_pre", StabilityNotLEQ(StabilityAccess(readByU), StabilityConstant(L())))),
      List(
        UpdatedHeap("_post", "_pre", List(writeByT))
      ))

    buffer.emit(s"""
                   |procedure Stable.Check.JKL.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | ${writeByT.decls()}
                   | ${readByU.decls()}
                   | ${readByUPost.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${readByU.eval(sprinter)}
                   | ${writeByT.eval(sprinter)}
                   |
                   | ${preCode}
                   | ${l.boogieAccess} := v;
                   | ${postCode}
                   |
                   | ${readByUPost.eval(sprinter)}
                   | ${if (l.spec.blocking) "assume _writeByT_Mover != _E;  // will block until it can go..." else ""}
                   |
                   |""".stripMargin)

    caseJ.gen(buffer)
    caseK.gen(buffer)
    caseL.gen(buffer)

    buffer.emit(s"""
                   |}
                   |""".stripMargin)


    buffer.emit()

  }



  def checkStabilityM(buffer: AnnotatedBuffer, l: MemoryAccess, k: MemoryAccess) = {
    val writeByT = WriteAccess(l, "t", "v", "_writeByT")
    val writeByU = WriteAccess(k, "u", "w1", "_writeByU")
    val writeByUAfterU = WriteAccess(k, "u", "w2", "_writeByUAfterU")
    val writeByUAfterTAndU = WriteAccess(k, "u", "w2", "_writeByUAfterTAndU")

    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(l.locType, "v") :: VarDecl(k.locType, "w1") :: VarDecl(k.locType, "w2") :: (l.parameters ++ k.parameters)

    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())
    val (postDecls, postCode) = sprinter.preserveState(parameters, "_post", "", new SinkStmtContext())
    val (midDecls, midCode) = sprinter.preserveState(parameters, "_mid", "", new SinkStmtContext())


    buffer.emit(s"""
                   |procedure Stable.Check.M.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | var tmpW: ${sprinter.pp(k.locType)};
                   | ${writeByU.decls()}
                   | ${writeByT.decls()}
                   | ${writeByUAfterU.decls()}
                   | ${writeByUAfterTAndU.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(midDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${writeByU.eval(sprinter)}
                   | ${writeByT.eval(sprinter)}
                   |
                   | ${preCode}
                   |
                   | tmpW := ${k.boogieAccess};
                   | ${k.boogieAccess} := w1;
                   |
                   | ${midCode}
                   |
                   | ${writeByUAfterU.eval(sprinter)}
                   | ${k.boogieAccess} := tmpW;
                   |
                   | ${l.boogieAccess} := v;
                   | ${k.boogieAccess} := w1;
                   |
                   | ${postCode}
                   |
                   | ${writeByUAfterTAndU.eval(sprinter)}
                   |
                   | ${if (l.spec.blocking) "assume _writeByT_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (k.spec.blocking) "assume _writeByUAfterU_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (k.spec.blocking) "assume _writeByUAfterTAndU_Mover != _E;  // will block until it can go..." else ""}
                   |
                   |""".stripMargin)

    val caseM = StabilityError(k, l, "Write-Write Stable", "M",
      List(
        StabilitySeqItem(writeByT, "_pre", StabilityLEQ(StabilityAccess(writeByT), StabilityConstant(N()))),
        StabilitySeqItem(writeByU, "_pre", StabilityNone()),
        StabilitySeqItem(writeByUAfterU, "_mid", StabilityNone()),
      ),
      List(
        StabilitySeqItem(writeByUAfterU, "_post", StabilityIn(StabilityAccess(writeByUAfterU), List(StabilityAccess(writeByUAfterTAndU), StabilityConstant(E()))))
      ),
      List(
        UpdatedHeap("_mid", "H", List(writeByU)),
        UpdatedHeap("_post", "H", List(writeByT, writeByU))
      ))

    caseM.gen(buffer)

    buffer.emit(s"""
                   |}
                   |""".stripMargin)

    buffer.emit()
    buffer.emit()
  }


  def checkStabilityN(buffer: AnnotatedBuffer, l: MemoryAccess, k: MemoryAccess) = {
    val writeByT = WriteAccess(l, "t", "v1", "_writeByT")
    val writeByU = WriteAccess(k, "u", "w", "_writeByU")
    val writeByTAfterT = WriteAccess(l, "t", "v2", "_writeByTAfterT")
    val writeByTAfterUAndT = WriteAccess(l, "t", "v2", "_writeByTAfterUAndT")

    val parameters =
      VarDecl(TidType(), "t") :: VarDecl(TidType(), "u") :: VarDecl(l.locType, "v1") :: VarDecl(l.locType, "v2") :: VarDecl(k.locType, "w") :: (l.parameters ++ k.parameters)

    val (preDecls, preCode) = sprinter.preserveState(parameters, "_pre", "", new SinkStmtContext())
    val (midDecls, midCode) = sprinter.preserveState(parameters, "_mid", "", new SinkStmtContext())
    val (postDecls, postCode) = sprinter.preserveState(parameters, "_post", "", new SinkStmtContext())

    val caseN =       StabilityError(k, l, "Write-Write Stable", "N",
      List(  StabilitySeqItem(writeByU, "_pre", StabilityLEQ(StabilityAccess(writeByU), StabilityConstant(L())))  ),
      List(
        StabilitySeqItem(writeByTAfterUAndT, "_post", StabilityIn(StabilityAccess(writeByTAfterUAndT),
        List(StabilityAccess(writeByTAfterT), StabilityConstant(E()))))),
      List(
        UpdatedHeap("_mid", "_pre", List(writeByT)),
        UpdatedHeap("_post", "_pre", List(writeByU, writeByT))
      )
    )


    buffer.emit(s"""
                   |procedure Stable.Check.N.${l.id}.${k.id}(${sprinter.genParams(parameters)})
                   |${newMethodSpec(l,k)}
                   |{
                   | var tmpV: ${sprinter.pp(l.locType)};
                   | ${writeByU.decls()}
                   | ${writeByT.decls()}
                   | ${writeByTAfterT.decls()}
                   | ${writeByTAfterUAndT.decls()}
                   | ${sprinter.genLocals(preDecls)}
                   | ${sprinter.genLocals(midDecls)}
                   | ${sprinter.genLocals(postDecls)}
                   |
                   | ${writeByU.eval(sprinter)}
                   |
                   | ${preCode}
                   |
                   | tmpV := ${l.boogieAccess};
                   | ${l.boogieAccess} := v1;
                   | ${writeByTAfterT.eval(sprinter)}
                   | ${l.boogieAccess} := tmpV;
                   |
                   | ${k.boogieAccess} := w;
                   | ${midCode}
                   | ${writeByT.eval(sprinter)}
                   | ${l.boogieAccess} := v1;
                   | ${writeByTAfterUAndT.eval(sprinter)}
                   |
                   | ${postCode}
                   |
                   | ${if (k.spec.blocking) "assume _writeByU_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (l.spec.blocking) "assume _writeByTAfterT_Mover != _E;  // will block until it can go..." else ""}
                   | ${if (l.spec.blocking) "assume _writeByTAfterUAndT_Mover != _E;  // will block until it can go..." else ""}
                   |
                   |""".stripMargin)

    caseN.gen(buffer)

    buffer.emit(s"""
                   |}
                   |""".stripMargin)

    buffer.emit()
    buffer.emit()


  }
}





