package anchor.sink

import anchor.sink.AST.pos
import anchor.transforms.{DeepCopyWithPositions, FixPosition}
import PrettyPrint._

import scala.util.parsing.input.Position

object ExplicitCAS {
  def apply(p : Program): Program = {
    new ExplicitCAS(p)()
  }
}

class ExplicitCAS(program: Program) extends DeepCopyWithPositions {

  val casFields = this.findCASables(program)

  def apply() : Program = {
    this.apply(program)
  }

  def isCASField(f : FieldDecl) = {
    val s =(f.parent.name, f.name)
    casFields.find(s => s._1 == f.parent.name && s._2 == f.name) != None
  }

  def casNextWriter(f: FieldDecl) = {
    s"${f.name}_nextThread"
  }
  def casNextValue(f: FieldDecl) = {
    s"${f.name}_nextValue"
  }

  def makeCASShadows(f: FieldDecl) : List[FieldDecl] = {
    def makeInternal(f : FieldDecl) = {
      FieldDecl(f.t, f.name, f.spec, f.modifiers :+ InternalModifier())
    }

    val shadowWriter = casNextWriter(f)
    val shadowValue = casNextValue(f)
    val fdWriter =
      Parser.fieldDecl(
        s"""Tid ${shadowWriter} isLocal(this) ?
           |    B :
           |    (this.${shadowWriter} == tid ? N : E)
           |  yields_as (this.${shadowWriter} == tid) ==> (newValue == tid) ; """.stripMargin
      )
    val fdValue =
      Parser.fieldDecl(
        s"""${pp(f.t)} ${shadowValue} isLocal(this) ?
           |    B :
           |    (this.${shadowWriter} == tid ? N : E)
           |  yields_as (this.${shadowWriter} == tid) ==> (newValue == this.${shadowValue}); """.stripMargin
      )
    List(FixPosition(f.pos)(makeInternal(fdWriter)), FixPosition(f.pos)(makeInternal(fdValue)))
  }

  override def apply(x: FieldDecl): FieldDecl = {
    if (this.isCASField(x)) {
      pos(FieldDecl(this (x.t), x.name, this (x.spec), pos(HasCASOperationModifier(), x.pos) :: x.modifiers.map(this (_))), x.pos)
    } else {
      pos(FieldDecl(this (x.t), x.name, this (x.spec), x.modifiers.map(this (_))), x.pos)
    }
  }

  override def apply(x: ClassDecl): ClassDecl = {
    pos(ClassDecl(x.name,
      x.arrays.map(this (_)),
      x.fields.map(this (_)) ++ x.fields.filter(x => isCASField(x)).flatMap(makeCASShadows(_)),
      x.methods.map(this (_)),
      x.invariants.map(this (_))),
      x.pos)
  }

  override def apply(x: MethodDecl): MethodDecl = {
    //    val requires = makeRequiresNoFutureCAS().map(FixPosition(x.pos)(_))
    val stmt = this (x.stmt)
    pos(MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), this(x.spec), stmt),
      x.pos)
  }

  override def apply(x: Stmt): Stmt = {
    x match {
      case x@Write(lhs, field, rhs, None) if (isCASField(x.decl)) => {
        FixPosition(x.pos)(Parser.block(
          s"""
             | {
             |   assume ${pp(lhs)}.${casNextWriter(x.decl)} == tid;
             |   ${pp(lhs)}.${x.decl.name} := ${pp(rhs)};
             | }
          """.stripMargin))
      }

      case x@Read(lhs, rhs, field, None) if (isCASField(x.decl)) => {
        FixPosition(x.pos)(Parser.block(
          s"""
             | {
             |   Tid _C_t := ${pp(rhs)}.${casNextWriter(x.decl)} as B;
             |   ${pp(x.decl.t)} _C_v := ${pp(rhs)}.${casNextValue(x.decl)} as B;
             |   ${pp(x.decl.t)} _currentValue := ${pp(rhs)}.${field} as B;
             |   Mover _R_t = permission(${pp(rhs)}.${x.decl.name});
             |   bool _casable = _R_t != E &&
             |                   ${x.decl.isABAFree} &&  // is ABA Free?
             |                   _C_t == tid && _C_v == _currentValue;
             |   if (_casable) {
             |     ${pp(lhs)} := ${pp(rhs)}.${field} as R;
             |   } else {
             |     ${pp(lhs)} := ${pp(rhs)}.${field};  // as _R_t
             |   }
             | }
             """.stripMargin
        ))
      }

      case x@CAS(result, lhs, field, expected, rhs) => {
        assert(isCASField(x.decl))
        FixPosition(x.pos)(Parser.block(
          s"""
             | {
             |   bool ctmp${x.id} = *; if (ctmp${x.id}) {
             |     // [Red CAS-]
             |     ${pp(result)} = false;
             |   } else {
             |     ctmp${x.id} = *; if (ctmp${x.id}) {
             |       // [Red CAS+]
             |
             |       Tid tmpTid;  // random next tid...
             |       ${pp(x.decl.t)} tmpValue;  // random next tid...
             |
             |       Mover _m = permission(${pp(lhs)}.${x.decl.name}, ${pp(rhs)});
             |
             |       assume ${pp(lhs)}.${field} == ${pp(expected)};
             |       assume !goesWrong(_m);
             |       assume ${pp(lhs)}.${casNextWriter(x.decl)} == tid;
             |       assume ${pp(lhs)}.${casNextValue(x.decl)} == ${pp(expected)};
             |
             |       ${pp(lhs)}.${field} := ${pp(rhs)};
             |
             |       ${pp(lhs)}.${casNextWriter(x.decl)} := tmpTid as B;
             |       ${pp(lhs)}.${casNextValue(x.decl)} := tmpValue as B;
             |
             |       ${pp(result)} = true;
             |     } else {
             |       // [Red CAS+ Wrong]
             |       assume ${pp(lhs)}.${casNextWriter(x.decl)} == tid;
             |       assume ${pp(lhs)}.${casNextValue(x.decl)} == ${pp(expected)};
             |       ${pp(x.decl.t)} _currentValue := ${pp(lhs)}.${field} as B;
             |       ${pp(lhs)}.${field} := ${pp(expected)} as B;
             |       Mover _m = permission(${pp(lhs)}.${x.decl.name}, ${pp(rhs)});
             |       ${pp(lhs)}.${field} := _currentValue as B;
             |       assume goesWrong(_m);
             |       ${pp(lhs)}.${field} := ${pp(rhs)};
             |       ${pp(result)} = false;
             |     }
             |   }
             | }
          """.stripMargin
        ))
      }
      case x@Return(e, isSynthetic)                           => {
        val assumes = for (f <- casFields) yield s"assume (forall ${f._1} _i :: _i.${f._2}_nextThread != tid);"
        FixPosition(x.pos)(Parser.block(
          s"""
             | {
             |   ${assumes.mkString("    \n")}
             |   ${pp(x)}
             | }
          """.stripMargin
        ))
        pos(Return(e.map(this (_)), isSynthetic), x)
      }
      case _                                                => super.apply(x)
    }
  }

  //////

  def findCASables(x: Program): List[(String,String)] = {
    val casFieldFinder = new FindFieldsWithCASOps()
    val fields = casFieldFinder(x).toList
    acme.scala.Util.log(s"CAS Fields: ${fields.map(x => x._1 + "." + x._2).mkString(", ")}")
    return fields
  }
}
