package anchor.tool

import anchor.sink._
import anchor.util.Errors

case class LexicalScope(val invoke : Option[Invoke], val invariants : List[Expr])

case class SinkStmtContext private(val method: Option[MethodDecl],
                              val locals : Set[VarDecl],
                              val inlinedStack : List[LexicalScope]) {

  val localNames = locals.map(_.name)

  def this(method: Option[MethodDecl]) {
    this(method, Set.empty, LexicalScope(None, Nil)::Nil)
  }

  def this(method : MethodDecl) {
    this(Some(method))
  }

  def this() {
    this(None)
  }

  def addVar(v : VarDecl): SinkStmtContext = {
    this.addVars(Set(v))
  }

  def addVars(vs : Set[VarDecl]) = {
    if (!vs.isEmpty) {
      if (!localNames.intersect(vs.map(_.name)).isEmpty) {
        for (v <- vs) {
          vs.find(_.name == v.name) match {
            case Some(value) => Errors.check("Sink", value.t == v.t, s"${v.name} is already in Sink Context -- alpha rename", vs.head)
            case None        => // same name, but also same type
          }
        }
      }
    }
    this.copy(locals = locals ++ vs)
  }

  def addInv(e : Expr) = {
    val LexicalScope(invoke, invs)::rest = inlinedStack
    this.copy(inlinedStack = LexicalScope(invoke, e::invs)::rest)
  }
  def addInvs(es : List[Expr]) = {
    val LexicalScope(invoke, invs)::rest = inlinedStack
    this.copy(inlinedStack = LexicalScope(invoke, es ++ invs)::rest)
  }

  def pushScope(invoke : Option[Invoke]) = {
    this.copy(inlinedStack = LexicalScope(invoke,Nil)::inlinedStack)
  }

  def popScope() = {
    val _::rest = inlinedStack
    this.copy(inlinedStack = rest)
  }

  def inlined = {
    inlinedStack.collect( { case LexicalScope(Some(i),_) => i })
  }

  def invariants = {
    inlinedStack.flatMap(_.invariants)
  }

  def topIsPublic = {
    method match {
      case Some(m) if m.isPublic => true
      case _ => false
    }
  }

}
