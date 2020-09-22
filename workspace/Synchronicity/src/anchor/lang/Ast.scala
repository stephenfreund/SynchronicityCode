package anchor.lang

import scala.util.parsing.input._

sealed trait ASTNode extends Positional {
  var scope : SymTab = null
}

case class NoNode() extends ASTNode {
}

object NoNode {
  def apply(pos: Position) : NoNode = {
    val n = NoNode()
    n.pos = pos
    n
  }
}

case class MoverSpec(val conditionalMover: Expr,
                     val blocking: Boolean,
                     val yieldsAs: List[Expr]) extends ASTNode {
  var nuDecl: VarDecl = null
}

sealed trait HasMoverSpec extends ASTNode {
  def spec : MoverSpec
}

case class Program(val classes : List[ClassDecl],
                   val globals : List[VarDecl],
                   val axioms : List[Expr]) extends ASTNode {
  def ++(other: Program): Program = {
    AST.pos(Program(this.classes ++ other.classes, this.globals ++ other.globals, this.axioms ++ other.axioms), this.pos)
  }
}

case class ClassInvariant(val pred: Expr, val triggers: List[List[Expr]]) extends ASTNode

case class ClassDecl(val name : String,
                     val arrays : List[ArrayDecl],
                     val fields : List[FieldDecl],
                     val methods : List[MethodDecl],
                     val constructor: ConstructorDecl,
                     val invariants : List[ClassInvariant]) extends ASTNode

trait ClassDeclElem

case class Transaction(val repeats: Boolean,
                       val modifies: List[Expr],
                       val ensures: List[Expr]) extends ASTNode

sealed abstract class MethodSpec extends ASTNode
case class InlineMethodSpec() extends MethodSpec
case class ExplicitMethodSpec(val requires : List[Expr],
                              val vars : List[VarDecl],
                              val transactions : List[Transaction]) extends MethodSpec

trait RoutineDecl {
  var parent : ClassDecl = null
  def isPublic: Boolean
  def name: String
  def params: List[VarDecl]
  def spec: ExplicitMethodSpec
  def stmt: Stmt
}

case class MethodDecl(val isPublic: Boolean, val returnType: Type, val name: String, val params: List[VarDecl], val spec: ExplicitMethodSpec, val stmt: Stmt) extends ASTNode with ClassDeclElem with RoutineDecl

case class ConstructorDecl(val isPublic: Boolean, val name: String, val params: List[VarDecl], val spec: ExplicitMethodSpec, val stmt: Stmt) extends ASTNode with ClassDeclElem with RoutineDecl


sealed abstract class FieldModifier extends ASTNode
case class VolatileModifier() extends FieldModifier
case class ABAFreeModifier() extends FieldModifier
case class GhostModifier() extends FieldModifier

case class FieldDecl(val t : Type,
                     val name : String,
                     val spec : MoverSpec,
                     val modifiers : List[FieldModifier]) extends ASTNode with HasMoverSpec with ClassDeclElem {
  var parent : ClassDecl = null
  def isVolatile = modifiers.contains(VolatileModifier())
  def isABAFree = modifiers.contains(ABAFreeModifier())
  def isGhost = modifiers.contains(GhostModifier())
}

case class ArrayDecl(val name : String,
                     val elemType : Type,
                     val elemName : String,
                     val spec : MoverSpec) extends ASTNode with HasMoverSpec with ClassDeclElem {
  var parent : ClassDecl = null
}

case class VarDecl(val t : Type,
                   val name : String) extends ASTNode

trait FieldReference{
  var decl : FieldDecl = null
}

object VarDeclStmt {
  def apply(decl : VarDecl, pos: Position) : VarDeclStmt = {
    val x : VarDeclStmt = VarDeclStmt(decl, None)
    x.pos = pos
    x
  }
}

sealed abstract class Stmt extends ASTNode

case class VarDeclStmt(val decl : VarDecl, rhs: Option[Expr]) extends Stmt
case class Assign(val lhs : Location, val rhs : Expr) extends Stmt
case class LocalAssign(val assigns : List[Assign]) extends Stmt
case class Block(val label : Option[String],
                 val body : List[Stmt]) extends Stmt
case class ExprStmt(e: Expr) extends Stmt

case class Return(val result : Option[Expr], val isSynthetic: Boolean) extends Stmt
case class If(val cond : Expr,
              val trueBranch : Stmt,
              val falseBranch : Stmt) extends Stmt
case class While(val cond : Expr,
                 val Stmt : Stmt,
                 val invariants: List[Expr],
                 val decreases: List[Expr]) extends Stmt
case class Break(val label : Option[String]) extends Stmt
case class Assume(val expr : Expr) extends Stmt
case class Assert(val expr : Expr) extends Stmt
case class Invariant(val expr : Expr) extends Stmt
case class Yield(val ensures: List[Expr]) extends Stmt
case class Commit() extends Stmt
case class BoogieCode(val code : String) extends Stmt

case class SyncBlock(val lock : Location, val stmt : Stmt, val releasePos: Position) extends Stmt
//case class Acquire(val lock : Location) extends Stmt
//case class Release(val lock : Location) extends Stmt

sealed abstract class SyncOp extends ASTNode
case class Acquire() extends SyncOp
case class Release() extends SyncOp
case class Wait() extends SyncOp
case class Notify() extends SyncOp

case class SyncStmt(val sync: SyncOp, val lock: Location) extends Stmt

case class NoReductionCheck(val stmt: Stmt) extends Stmt

sealed abstract class Const extends ASTNode {
}
case class IntConst(val v : Int) extends Const {
}
case class BoolConst(val v : Boolean) extends Const {
}
case class NullConst() extends Const {
  var t : Type = null
}
case class NullTid() extends Const {
}

/// Specs

sealed abstract class Mover {
  def <=(o: Mover): Boolean = {
    this match {
      case I() => true
      case B() => o != I()
      case R() => o == R() || o == N() || o == E()
      case L() => o == L() || o == N() || o == E()
      case N() => o == N() || o == E()
      case E() => o == E()
    }
  }
}

case class I() extends Mover
case class B() extends Mover
case class R() extends Mover
case class L() extends Mover
case class N() extends Mover
case class E() extends Mover

case class MoverConst(val m : Mover) extends Const {
}

case class EmptyCollectionConst(val t : CollectionType) extends Const {

}

sealed abstract class Expr extends ASTNode with ClassDeclElem {
  var t : Type = null
}
case class ConstExpr(val const : Const) extends Expr
case class BinaryExpr(val lhs : Expr, val rhs : Expr, val op : BinaryOp) extends Expr
case class UnaryExpr(val expr : Expr, val op : UnaryOp) extends Expr

case class Invoke(val ref : Expr,
                  val method : String,
                  val args : List[Expr],
                  val invariants : List[Expr]) extends Expr {
  var decl : MethodDecl = null
}


sealed abstract class Location extends Expr
case class VarAccess(val name : String) extends Location  {
  var decl : VarDecl = null
  def this(decl : VarDecl) {
    this(decl.name)
    this.decl = decl
  }
}
case class FieldAccess(val l : Expr, val name : String, val isStable : Boolean = true) extends Location with FieldReference

case class CAS(val ref : Expr,
               val field : String,
               val expected: Expr,
               val rhs : Expr) extends Expr with FieldReference


case class Old(val l : Expr) extends Expr

case class ArrayAccess(val l : Expr, val index : Expr) extends Location

sealed abstract class Quantifier
case class ForAll() extends Quantifier
case class Exists() extends Quantifier

case class Quantified(quantifier: Quantifier, val decls: List[VarDecl], val pred: Expr, val triggers: List[List[Expr]]) extends Expr
case class Cond(p : Expr, tt : Expr, ff : Expr) extends Expr


trait MathOperation
trait BooleanOperation
sealed abstract class BinaryOp extends ASTNode
case class Add() extends BinaryOp with MathOperation
case class Sub() extends BinaryOp with MathOperation
case class Mul() extends BinaryOp with MathOperation
case class Div() extends BinaryOp with MathOperation
case class Mod() extends BinaryOp with MathOperation
case class And() extends BinaryOp with BooleanOperation
case class Or() extends BinaryOp with BooleanOperation

trait MathComparison
trait EqualComparison
trait OrderComparison
case class EQ() extends BinaryOp with MathComparison with EqualComparison
case class NE() extends BinaryOp with MathComparison with EqualComparison
case class LT() extends BinaryOp with MathComparison with OrderComparison
case class GT() extends BinaryOp with MathComparison with OrderComparison
case class LE() extends BinaryOp with MathComparison with OrderComparison
case class GE() extends BinaryOp with MathComparison with OrderComparison
case class Implies() extends BinaryOp with BooleanOperation

sealed abstract class UnaryOp extends ASTNode
case class Not() extends UnaryOp
case class Neg() extends UnaryOp
case class Paren() extends UnaryOp

sealed abstract class PrimitiveFunction extends Expr
case class Length(val loc: Expr) extends PrimitiveFunction
case class Lock(val loc: Expr) extends PrimitiveFunction
case class IsLocal(val loc: Expr, val tid: Expr) extends PrimitiveFunction
case class IsShared(val loc: Expr) extends PrimitiveFunction
case class IsFresh(val loc: Expr) extends PrimitiveFunction
case class Holds(val loc: Expr, val tid: Expr) extends PrimitiveFunction
case class NextCASSucceeds(val loc: Expr, val tid: Expr) extends PrimitiveFunction
case class Rand() extends PrimitiveFunction
case class NextSpecStep(val step: Int) extends PrimitiveFunction

// do we want invariants on alloc since it now calls the cstr?
case class Alloc(val name : ClassType, val args : List[Expr], val invariants : List[Expr]) extends PrimitiveFunction {
  var decl : ConstructorDecl = null
}
case class AAlloc(val a : ArrayType, val size : Expr) extends PrimitiveFunction

sealed abstract class Type extends ASTNode
final case class IntType() extends Type
final case class TidType() extends Type
final case class VoidType() extends Type
final case class BoolType() extends Type
final case class MoverType() extends Type
final case class BoogieType(val name: String) extends Type



sealed abstract class RefType extends Type


final case class ClassType(val name : String) extends RefType {
  var decl : ClassDecl = null
}

object ClassType {
  def apply(decl : ClassDecl) : ClassType = {
    val x = new ClassType(decl.name)
    x.decl = decl
    x
  }
}

final case class ArrayType(val prefix : String, ident : String, val thisAccess : VarAccess) extends RefType {
  var decl : ArrayDecl = null

  def elemType() : Type = {
    decl.elemType
  }
}


case class TypeVar(val n : String) extends Type

case class CollectionType(val name : String, val typeArgs : List[Type]) extends Type {
  var decl : Collection = null
  def defaultValue() = s"${name}#Empty : ${name} ${typeArgs.map(PrettyPrint.pp(_)).mkString(" ")}"
}

case class BuiltInFunctionDecl(val name : String,
                               val typeVars : List[String],
                               val parameters : List[Type],
                               val returnType : Type) extends ASTNode

case class BuiltInFunctionCall(val name : String,
                               val types : Option[List[Type]],
                               val arguments : List[Expr]) extends Expr {
  var decl : BuiltInFunctionDecl = null
  var inferredTypes : List[Type] = null
}

object Type {
  def isValidFieldOrVarType(t : Type, isThis : Boolean = false) = {
    t != VoidType() && !t.isInstanceOf[BoogieType]
  }

  def isHeapReference(t : Type) = {
    t match {
      case ClassType(_) => true
      case ArrayType(_, _, _) => true
      case _ => false
    }
  }

  def isValidReturnType(t : Type) = {
    true // t != NullType()
  }

  def isObject(t : Type) = {
    t.isInstanceOf[ClassType]
  }

  def isArray(t : Type) = {
    t.isInstanceOf[ArrayType]
  }

  def isAssignmentConvertible(from : Type, to : Type): Boolean = {
    (from,to) match {
      case (x,y) if x == y => true
      case (x,y) if isIntType(x) && isIntType(y) => true
      case (x:ClassType, y:ClassType) => x.decl eq y.decl
      case (x:ArrayType, y:ArrayType) => (x.decl eq y.decl) && x.thisAccess == y.thisAccess
      case (x:CollectionType, y:CollectionType) => (x.decl eq y.decl) && x.typeArgs.zip(y.typeArgs).forall(a=>isAssignmentConvertible(a._1, a._2))
      case _ => false
    }
  }

  def isIntType(t : Type) = {
    t.isInstanceOf[IntType] || t.isInstanceOf[TidType]
  }
}

object AST {
  def and(lhs : Expr, rhs : Expr): Expr = {
    (lhs,rhs) match {
      case(ConstExpr(BoolConst(false)),_) => ConstExpr(BoolConst(false))
      case(_,ConstExpr(BoolConst(false))) => ConstExpr(BoolConst(false))
      case(ConstExpr(BoolConst(true)),_) => rhs
      case(_,ConstExpr(BoolConst(true))) => lhs
      case(p,UnaryExpr(q, Not())) if p == q => ConstExpr(BoolConst(false))
      case(UnaryExpr(p, Not()), q) if p == q => ConstExpr(BoolConst(false))
      case(_,_) => BinaryExpr(lhs, rhs, And())
    }
  }
  def or(lhs : Expr, rhs : Expr): Expr = {
    (lhs,rhs) match {
      case(ConstExpr(BoolConst(false)),_) => rhs
      case(_,ConstExpr(BoolConst(false))) => lhs
      case(ConstExpr(BoolConst(true)),_) => ConstExpr(BoolConst(true))
      case(_,ConstExpr(BoolConst(true))) => ConstExpr(BoolConst(true))
      case(p,UnaryExpr(q, Not())) if p == q => ConstExpr(BoolConst(true))
      case(UnaryExpr(p, Not()), q) if p == q => ConstExpr(BoolConst(true))
      case(_,_) => BinaryExpr(lhs, rhs, Or())
    }
  }
  def not(rhs : Expr): Expr = {
    rhs match {
      case ConstExpr(BoolConst(false)) => ConstExpr(BoolConst(true))
      case ConstExpr(BoolConst(true)) => ConstExpr(BoolConst(false))
      case _ => UnaryExpr(rhs, Not())
    }
  }

  def pos[T <: Positional](t : T, pos: Position) : T = {
    t.pos = pos
    t
  }

  def pos[T <: Positional](t : T, u : Positional) : T = {
    pos(t, u.pos)
  }
}