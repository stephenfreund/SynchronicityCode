//package anchor.sink
//
//import anchor.sink.AST.pos
//import anchor.transforms.{DeepCopyWithPositions, FixPosition}
//import PrettyPrint._
//
//import scala.util.parsing.input.Position
//
//object ExplicitCAS {
//  def apply(p : Program): Program = {
//    new ExplicitCAS(p)()
//  }
//}
//
//class ExplicitCAS(program: Program) extends DeepCopyWithPositions {
//
//  val casFields = this.findCASables(program)
//
//  def apply() : Program = {
//    this.apply(program)
//  }
//
//  def isCASField(f : FieldDecl) = {
//    casFields.find(_ eq f) != None
//  }
//
//  def casNextWriter(f: FieldDecl) = {
//    s"${f.name}_nextThread"
//  }
//
//  def makeCASShadows(f: FieldDecl) : FieldDecl = {
//    def makeInternal(f : FieldDecl) = {
//      val inv =
//        Parser.expr(
//          //s"""isLocal(this) ==> (this.${f.name} == Tid.null)""".stripMargin
//          "true"
//        )
//      FixPosition(f.pos)(inv)
//
//      FieldDecl(f.t, f.name, f.spec, f.modifiers :+ InternalModifier(inv))
//    }
//
//    val shadowWriter = casNextWriter(f)
//    val fdWriter =
//      Parser.fieldDecl(
//        s"""Tid ${shadowWriter} isLocal(this) ?
//           |    B :
//           |    (this.${shadowWriter} == tid ? N : E)
//           |  yields_as (this.${shadowWriter} == tid) ==> (newValue == tid) ; """.stripMargin
//      )
//    FixPosition(f.pos)(makeInternal(fdWriter))
//  }
//
//  override def apply(x: ClassDecl): ClassDecl = {
//    pos(ClassDecl(x.name,
//      x.arrays.map(this (_)),
//      x.fields.map(this (_)) ++ x.fields.intersect(casFields).map(makeCASShadows(_)),
//      x.methods.map(this (_)),
//      x.invariants.map(this (_))),
//      x.pos)
//  }
//
//  override def apply(x: MethodDecl): MethodDecl = {
//    //    val requires = makeRequiresNoFutureCAS().map(FixPosition(x.pos)(_))
//    val stmt = this (x.stmt)
//    pos(MethodDecl(x.isPublic,
//      this (x.returnType),
//      x.name,
//      x.params.map(this (_)),
//      x.requires.map(this (_)),
//      stmt),
//      x.pos)
//  }
//
//  override def apply(x: Stmt): Stmt = {
//    x match {
//      case x@Write(lhs, field, rhs, None) if (isCASField(x.decl)) => {
//        FixPosition(x.pos)(Parser.block(
//          s"""
//             | {
//             |   Mover _m = permission(${pp(lhs)}.${x.decl.name});
//             |   if (!goesWrong(_m)) {
//             |     // [Red Write]
//             |     Tid tmpTid;
//             |     assume ${pp(lhs)}.${casNextWriter(x.decl)} == Tid.null;   // C[rho.f] = \bot
//             |     ${pp(lhs)}.${x.decl.name} := ${pp(rhs)};
//             |     ${pp(lhs)}.${casNextWriter(x.decl)} := tmpTid as B;
//             |   } else {
//             |     // [Red Write Wrong]
//             |     ${pp(lhs)}.${x.decl.name} := ${pp(rhs)};  // will go wrong
//             |   }
//             | }
//          """.stripMargin))
//      }
//
//      case x@Read(lhs, rhs, field, None) if (isCASField(x.decl)) => {
//        FixPosition(x.pos)(Parser.block(
//          s"""
//             | {
//             |   Tid _C_t := ${pp(rhs)}.${casNextWriter(x.decl)} as B;
//             |   Mover _R_t = permission(${pp(rhs)}.${x.decl.name});
//             |   bool _casable = (_C_t == tid &&
//             |                    ${x.decl.isABAFree} &&
//             |                    _R_t != E);
//             |   if (_casable) {
//             |     ${pp(lhs)} := ${pp(rhs)}.${field} as R;
//             |   } else {
//             |     ${pp(lhs)} := ${pp(rhs)}.${field};  // as _R_t
//             |   }
//             | }
//             """.stripMargin
//        ))
//      }
//
//      case x@CAS(result, lhs, field, expected, rhs) => {
//        assert(isCASField(x.decl))
//        FixPosition(x.pos)(Parser.block(
//          s"""
//             | {
//             |   bool tmp${x.id} = *; if (tmp${x.id}) {
//             |     // [Red CAS-]
//             |     ${pp(result)} = false;
//             |   } else {
//             |     assume ${pp(lhs)}.${field} == ${pp(expected)};
//             |     Mover _m = permission(${pp(lhs)}.${x.decl.name}, ${pp(rhs)});
//             |     if (!goesWrong(_m)) {
//             |       // [Red CAS+]
//             |       Tid tmpTid;  // random next tid...
//             |       assume ${pp(lhs)}.${casNextWriter(x.decl)} == tid;
//             |       ${pp(lhs)}.${field} := ${pp(rhs)};
//             |       ${pp(lhs)}.${casNextWriter(x.decl)} := tmpTid as B;
//             |       ${pp(result)} = true;
//             |     } else {
//             |       // [Red CAS+ Wrong]
//             |       assume ${pp(lhs)}.${casNextWriter(x.decl)} == tid || _m == E;
//             |       ${pp(lhs)}.${field} := ${pp(rhs)};
//             |     }
//             |   }
//             | }
//          """.stripMargin
//        ))
//      }
//      case x@Return(e, isSynthetic)                           => {
//        val assumes = for (f <- casFields) yield s"assume (forall ${f.parent.name} _i :: _i.${casNextWriter(f)} != tid);"
//        FixPosition(x.pos)(Parser.block(
//          s"""
//             | {
//             |   ${assumes.mkString("    \n")}
//             |   ${pp(x)}
//             | }
//          """.stripMargin
//        ))
//        pos(Return(e.map(this (_)), isSynthetic), x)
//      }
//      case _                                                => super.apply(x)
//    }
//  }
//
//  //////
//
//  def findCASables(x: Program): List[FieldDecl] = {
//    val casFieldFinder = new FindFieldsWithCASOps()
//    val fields = casFieldFinder(x).toList
//    acme.scala.Util.log(s"CAS Fields: ${fields.map(x => x.parent.name + "." + x.name).mkString(", ")}")
//    return fields
//  }
//}
