package anchor.sink

import anchor.sink
import anchor.tool.{ArrayMemoryAccess, FieldMemoryAccess}
import anchor.transforms.DeepCopyWithPositions

import scala.util.parsing.input._

object ASTNode {
  private var count : Long = 0;
  private def unique() : Long = {
    synchronized {
      count += 1
      count
    }
  }
}

sealed trait ASTNode extends Positional {
  val id: Long = ASTNode.unique()
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

case class Spec(val conditionalMover: Expr,
                val blocking: Boolean,
                val yieldsAs: List[Expr]) extends ASTNode {
  var nuDecl: VarDecl = null
}

sealed trait HasSpec extends ASTNode {
  def spec : Spec
}

case class Program(val classes : List[ClassDecl],
                   val globals : List[VarDecl],
                   val axioms : List[Expr],
                   val library: Library) extends ASTNode

case class ClassInvariant(val pred: Expr, val triggers: List[List[Expr]]) extends ASTNode

case class ClassDecl(val name : String,
                     val arrays : List[ArrayDecl],
                     val fields : List[FieldDecl],
                     val methods : List[MethodDecl],
                     val invariants : List[ClassInvariant]) extends ASTNode


case class Transaction(val repeats: Boolean,
                       val modifies: List[Expr],
                       val ensures: List[Expr]) extends ASTNode

sealed abstract class MethodSpec extends ASTNode
case class InlineMethodSpec() extends MethodSpec
case class ExplicitMethodSpec(val requires : List[Expr],
                              val vars : List[VarDecl],
                              val transactions : List[Transaction]) extends MethodSpec {

  def hasEnsures() = transactions.size > 0

  private var cachedSpecDFA : SpecNFA = null
  def specDFA : SpecNFA = {
    assert(hasEnsures())
    if (cachedSpecDFA == null) {
      val nfa = new SpecNFA(this)
      cachedSpecDFA = nfa.epsilonFree()
    }
    cachedSpecDFA
  }

  def specSize = {
    if (hasEnsures()) specDFA.size else 0
  }

  def specLocalVarDecls(prefix:String="", suffix:String = "") : List[VarDecl] = {
    vars.map(v => VarDecl(v.t, prefix + v.name + suffix))
  }

  def specStateVarDecls(prefix:String="", suffix:String = "") : List[VarDecl] = {
    specStateVarNames(prefix, suffix).map(v => VarDecl(BoolType(), v))
  }

  def specStateVarNames(prefix:String="", suffix: String = "") : List[String] = {
    (0 until specSize).map(i => specStateVarName(i, prefix, suffix)).toList
  }

  def specStateVarNameTuple(prefix:String="", suffix: String = ""): String = {
    (0 until specSize).map(i => s"${specStateVarName(i, prefix, suffix)}").mkString(", ")
  }

  def specStateVarDeclTuple(prefix:String="", suffix: String = ""): String = {
    (0 until specSize).map(i => s"${specStateVarName(i, prefix, suffix)} : bool").mkString(", ")
  }

  def isInAcceptState() = {
    specDFA.acceptingStates().map(s => specStateVarName(s.n)).mkString(" || ")
  }

  def specStateVarName(i : Int, prefix:String ="", suffix: String = "") = {
    s"${prefix}$$spec$$pc.${i}$$${suffix}"
  }

  // returns (state, label)
  def transitionsInto(dst: Int) : List[(Int, Int)] = {
    specDFA.into(State(dst)).map(x => (x._1.n, x._2 match { case Step(i) => i })).toList
  }

  def stateForNextStep(step: Int) : String = {
    if (step < transactions.size) {
      val l = List(Step(step))
      val n = specDFA.states().find {
        src => {
          specDFA.outEdges(src).map(_._1) == l
        }
      }.get.n
      specStateVarName(n)
    } else {
      specStateVarName(specDFA.acceptingStates().toList.head.n)
    }
  }

}

case class MethodDecl(val isPublic: Boolean, val returnType: Type, val name: String, val params: List[VarDecl], val spec: ExplicitMethodSpec, val stmt: Stmt) extends ASTNode {
  var parent : ClassDecl = null
  private var cachedCFG : ControlFlowGraph = null
  def cfg : ControlFlowGraph = {
    if (cachedCFG == null) {
      cachedCFG = new ControlFlowGraph(this)
    }
    cachedCFG
  }

}

sealed abstract class FieldModifier extends ASTNode
case class VolatileModifier() extends FieldModifier
case class ABAFreeModifier() extends FieldModifier
case class InternalModifier() extends FieldModifier
case class HasCASOperationModifier() extends FieldModifier


case class FieldDecl(val t : Type,
                     val name : String,
                     val spec : Spec,
                     val modifiers : List[FieldModifier]) extends ASTNode with HasSpec {
  var parent : ClassDecl = null
  def isVolatile = modifiers.contains(VolatileModifier())
  def isABAFree = modifiers.contains(ABAFreeModifier())
  def isInternal = modifiers.contains(InternalModifier())
  def hasCASOperation = modifiers.contains(HasCASOperationModifier())

}

case class ArrayDecl(val name : String,
                     val elemType : Type,
                     val elemName : String,
                     val spec : Spec) extends ASTNode with HasSpec {
  var parent : ClassDecl = null
}


///


case class VarDecl(val t : Type,
                   val name : String) extends ASTNode

trait FieldReference{
  var decl : FieldDecl = null
}

object VarDeclStmt {
  def apply(decl : VarDecl, pos: Position) : VarDeclStmt = {
    val x : VarDeclStmt = VarDeclStmt(decl)
    x.pos = pos
    x
  }
}

sealed abstract class Stmt extends ASTNode
sealed trait Moveable extends ASTNode {
  def spec : Spec
}

case class VarDeclStmt(val decl : VarDecl) extends Stmt
case class Assign(val lhs : List[VarAccess],
                  val rhs : List[Expr]) extends Stmt

object Assign {
  def apply(lhs : VarAccess, rhs: Expr): Assign = Assign(List(lhs), List(rhs))
}

case class Block(val label : Option[String],
                 val body : List[Stmt]) extends Stmt


case class Write(val ref : VarAccess,
                 val field : String,
                 val rhs : VarOrConst,
                 val movesAs: Option[Mover] = None) extends Stmt with FieldReference with Moveable {
  def spec = new FieldMemoryAccess(this.decl, ref.name).spec
}

case class LocalWrites(val writes : List[Write]) extends Stmt

case class Read(val lhs : VarAccess,
                val ref : VarAccess,
                val field : String,
                val movesAs: Option[Mover] = None) extends Stmt with FieldReference with Moveable {
  def spec = new FieldMemoryAccess(this.decl, ref.name).spec
}

case class AWrite(val ref : VarAccess,
                  val index : VarAccess,
                  val rhs : VarOrConst) extends Stmt with Moveable {
  def spec = new ArrayMemoryAccess(this.ref.decl.t.asInstanceOf[ArrayType].decl, ref.name, index.name).spec
}

case class ARead(val lhs : VarAccess, val ref : VarAccess, val index : VarAccess) extends Stmt with Moveable  {
  def spec = new ArrayMemoryAccess(this.ref.decl.t.asInstanceOf[ArrayType].decl, ref.name, index.name).spec
}

case class CAS(val result: VarAccess,
               val ref : VarAccess,
               val field : String,
               val expected: VarOrConst,
               val rhs : VarOrConst) extends Stmt with FieldReference with Moveable {
  def spec = new FieldMemoryAccess(this.decl, ref.name).spec
}

case class Invoke(val ref : VarAccess,
                  val method : String,
                  val args : List[Expr],
                  val result : Option[VarAccess],
                  val invariants : List[Expr]) extends Stmt {
  var decl : MethodDecl = null
}

case class Return(val result : Option[VarOrConst], val isSynthetic: Boolean) extends Stmt
case class If(val cond : Expr,
              val trueBranch : Stmt,
              val falseBranch : Stmt) extends Stmt
case class While(val cond : Expr,
                 val Stmt : Stmt,
                 val invariants: List[Expr],
                 val decreases : List[Expr]) extends Stmt
case class Break(val label : Option[String]) extends Stmt
case class Alloc(val lhs : VarAccess,
                 val name : ClassType) extends Stmt
case class AAlloc(val lhs : VarAccess,
                  val t : ArrayType,
                  val size : VarOrConst) extends Stmt
case class Assume(val expr : Expr) extends Stmt
case class Assert(val expr : Expr) extends Stmt
case class Invariant(val expr : Expr) extends Stmt
case class Yield(val ensures: List[Expr]) extends Stmt
case class Commit() extends Stmt
case class BoogieCode(val code : String) extends Stmt

case class NoReductionCheck(val stmt: Stmt) extends Stmt

case class Sync(val lock : VarAccess, val stmt : Stmt, val releasePos: Position) extends Stmt
case class Acquire(val lock : VarAccess) extends Stmt with Moveable {
  def spec = Spec(ConstExpr(MoverConst(R())), true, List())
}
case class Release(val lock : VarAccess) extends Stmt with Moveable{
  def spec = Spec(ConstExpr(MoverConst(L())), false, List())
}

case class InlineInvoke(val invoke : Invoke) extends Stmt
case class InlineReturn() extends Stmt


//case class Inlined

sealed abstract class Const extends ASTNode {
  def typeOf() : Type
}
case class IntConst(val v : Int) extends Const {
  override def typeOf(): Type = IntType()
}
case class BoolConst(val v : Boolean) extends Const {
  override def typeOf(): Type = BoolType()
}
case class NullConst(val t : RefType) extends Const {
  override def typeOf(): Type = t
}
case class NullTid() extends Const {
  override def typeOf(): Type = TidType()
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

case class B() extends Mover
case class R() extends Mover
case class L() extends Mover
case class N() extends Mover
case class E() extends Mover
case class I() extends Mover

case class MoverConst(val m : Mover) extends Const {
  override def typeOf() : Type = MoverType()
}

case class EmptyCollectionConst(val t : CollectionType) extends Const {
  override def typeOf(): Type = t
}


sealed abstract class Expr extends ASTNode {
  var t : Type = null
}

case class Old(val l : Expr) extends Expr

sealed abstract trait VarOrConst extends Expr

case class ConstExpr(val const : Const) extends Expr with VarOrConst

case class BinaryExpr(val lhs : Expr, val rhs : Expr, val op : BinaryOp) extends Expr
case class UnaryExpr(val expr : Expr, val op : UnaryOp) extends Expr

case class LoweredExpr(val e: Expr, val original: Expr) extends Expr

object LoweredExpr {
  def apply(e : Expr, o : Option[Expr]): Expr = {
    o match {
      case Some(value) => {
        LoweredExpr(e, value)
      }
      case None        => {
        e
      }
    }
  }
}



sealed abstract class Location extends Expr

case class VarAccess(val name : String) extends Location with VarOrConst  {
  var decl : VarDecl = null
  def this(decl : VarDecl) {
    this(decl.name)
    this.decl = decl
  }
}
case class FieldAccess(val l : Expr, val name : String) extends Location with FieldReference
case class ArrayAccess(val l : Expr, val index : Expr) extends Location



//object Old {
//  def apply(e: Expr): Expr = {
//    AST.pos(e match {
//      case x@ConstExpr(const)              => DeepCopyWithPositions(x)
//      case x@BinaryExpr(lhs, rhs, op)      => BinaryExpr(this (lhs), this (rhs), op)
//      case x@UnaryExpr(expr, op)           => UnaryExpr(this (expr), op)
//      case x: Location                     => Old(x)
//      case Length(loc)                     => Length(loc)
//      case Lock(loc)                       => Lock(this(loc))
//      case IsLocal(loc, tid)               => IsLocal(this (loc), tid)
//      case IsShared(loc)                   => IsShared(this (loc))
//      case Holds(loc, tid)                 => Holds(this (loc), tid)
//      case MoverPermission(loc, v)       => MoverPermission(this (loc), v.map(apply))
//      case GoesWrong(mover)       => GoesWrong(this (mover))
//      case Rand()                          => Rand()
//      case NextSpecStep(n) => NextSpecStep(n)
//      case x@Quantified(q, decls, pred, triggers) => Quantified(q, decls, this (pred), triggers.map(_.map(this (_))))
//      case x@Cond(p, tt, ff)               => Cond(this (p), this (tt), this (ff))
//      case x@LoweredExpr(e, original)      => LoweredExpr(this (e), this (original))
//    }, e)
//  }
//}

sealed abstract class BinaryOp extends ASTNode
case class Add() extends BinaryOp
case class Sub() extends BinaryOp
case class Mul() extends BinaryOp
case class Div() extends BinaryOp
case class Mod() extends BinaryOp
case class And() extends BinaryOp
case class Or() extends BinaryOp

trait MathComparison
trait EqualComparison
case class EQ() extends BinaryOp with MathComparison with EqualComparison
case class NE() extends BinaryOp with MathComparison with EqualComparison
case class LT() extends BinaryOp with MathComparison
case class GT() extends BinaryOp with MathComparison
case class LE() extends BinaryOp with MathComparison
case class GE() extends BinaryOp with MathComparison
case class Implies() extends BinaryOp

sealed abstract class UnaryOp extends ASTNode
case class Not() extends UnaryOp
case class Neg() extends UnaryOp
case class Paren() extends UnaryOp


sealed abstract class PrimitiveFunction extends Expr
case class Length(val a: Expr) extends PrimitiveFunction
case class Lock(val a: Expr) extends PrimitiveFunction
case class IsLocal(val loc: Expr, val tid: Expr) extends PrimitiveFunction
case class IsShared(val loc: Expr) extends PrimitiveFunction
case class IsFresh(val loc: Expr) extends PrimitiveFunction
case class Holds(val loc: Expr, val tid: Expr) extends PrimitiveFunction
case class MoverPermission(val loc: Expr, val writeValue: Option[Expr]) extends PrimitiveFunction
case class GoesWrong(mover: Expr) extends PrimitiveFunction
case class Rand() extends PrimitiveFunction
case class NextSpecStep(val step: Int) extends PrimitiveFunction

sealed abstract class Type extends ASTNode
final case class IntType() extends Type
final case class TidType() extends Type
final case class VoidType() extends Type
final case class BoolType() extends Type
final case class MoverType() extends Type
final case class BoogieType(val name: String) extends Type

sealed abstract class Quantifier
case class ForAll() extends Quantifier
case class Exists() extends Quantifier

case class Quantified(quantifier: Quantifier, val decls: List[VarDecl], val pred: Expr, val triggers: List[List[Expr]]) extends Expr

case class Cond(p : Expr, tt : Expr, ff : Expr) extends Expr


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
}

case class BuiltInFunctionDecl(val name : String,
                               val typeVars : List[String],
                               val parameters : List[Type],
                               val returnType : Type) extends ASTNode

case class BuiltInFunctionCall(val name : String,
                               val types : List[Type],
                               val arguments : List[Expr]) extends Expr {
  var decl : BuiltInFunctionDecl = null
}

object Type {
  def isValidFieldOrVarType(t : Type, isThis : Boolean = false) = {
    t != VoidType()//  && !t.isInstanceOf[BoogieType]
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

  def isAssignmentConvertible(from : Type, to : Type): Boolean  = {
    (from,to) match {
      case (x,y) if x == y => true
      case (x,y) if isIntType(x) && isIntType(y) => true
      case (x:ClassType, y:ClassType) => x.decl eq y.decl
      case (x:ArrayType,y:ArrayType) => (x.decl eq y.decl) && x.thisAccess == y.thisAccess
      case (x:CollectionType, y:CollectionType) => (x.decl eq y.decl) && x.typeArgs.zip(y.typeArgs).forall(a=>isAssignmentConvertible(a._1, a._2))
      case _ => false
    }
  }

  def isIntType(t : Type) = {
    t.isInstanceOf[IntType] || t.isInstanceOf[TidType]
  }
}

object AST {
  def binary(lhs : Expr, rhs : Expr, op: BinaryOp) = {
    op match {
      case And()     => and(lhs, rhs)
      case Or()      => or(lhs, rhs)
      case EQ()      => if (lhs == rhs) ConstExpr(BoolConst(true)) else BinaryExpr(lhs, rhs, op)
      case Implies() => implies(lhs, rhs)
      case _ => BinaryExpr(lhs, rhs, op)
    }
  }

  def unary(rhs: Expr, op: UnaryOp) = {
    op match {
      case Not() => not(rhs)
      case Neg() => neg(rhs)
      case Paren() => UnaryExpr(rhs, Paren())
    }
  }

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

  def implies(lhs : Expr, rhs : Expr): Expr = {
    (lhs,rhs) match {
      case(ConstExpr(BoolConst(false)),_) => ConstExpr(BoolConst(true))
      case(ConstExpr(BoolConst(true)),rhs) => rhs
      case(_,_) => BinaryExpr(lhs, rhs, Implies())
    }
  }

  def not(rhs : Expr): Expr = {
    rhs match {
      case ConstExpr(BoolConst(false)) => ConstExpr(BoolConst(true))
      case ConstExpr(BoolConst(true)) => ConstExpr(BoolConst(false))
      case BinaryExpr(lhs, rhs, NE()) => BinaryExpr(lhs, rhs, EQ())
      case BinaryExpr(lhs, rhs, EQ()) => BinaryExpr(lhs, rhs, NE())
      case UnaryExpr(rhs, Not())      => rhs
      case _ => UnaryExpr(rhs, Not())
    }
  }

  def neg(rhs : Expr): Expr = {
    rhs match {
      case ConstExpr(IntConst(n)) => ConstExpr(IntConst(-n))
      case UnaryExpr(rhs, Neg())      => rhs
      case _ => UnaryExpr(rhs, Neg())
    }
  }

  def pos[T <: Positional](t : T, pos: Position) : T = {
    t.pos = pos
    t
  }

  def pos[T <: Positional](t : List[T], pos : Position) : List[T] = {
    t.foreach(_.pos = pos)
    t
  }

  def pos[T <: Positional](t : T, u : Positional) : T = {
    pos(t, u.pos)
  }

  def pos[T <: Positional](t : List[T], u : Positional) : List[T] = {
    t.map(pos(_, u.pos))
  }
}