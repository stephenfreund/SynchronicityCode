package anchor.tool

import anchor.sink._
import anchor.sink.PrettyPrint._
import anchor.transforms._
import anchor.util.Errors.check


sealed abstract class GuardedCommand

case class Skip() extends GuardedCommand
case class Update(loc: Location, e: Expr) extends GuardedCommand
case class GuardedStmt(p: Expr, s: GuardedCommand) extends GuardedCommand
case class Do(conds: List[GuardedStmt]) extends GuardedCommand

object GuardedCommand {

  private def wpHelper(s: GuardedCommand, Q: Expr) : Expr = {
    s match {
      case Skip()            => {
        Q
      }
      case Update(x, e)      => {
        val subst = Substitution(x,e)
        new Substitute(List(subst))(Q)
      }
      case GuardedStmt(p, s) => {
        AST.or(
          AST.and(p, wpHelper(s,Q)),
          AST.and(AST.not(p), Q))
      }
      case Do(conds)         => {
        val wps = conds.map(wpHelper(_, Q))
        wps.foldRight(Q)((acc, wp) => AST.or(acc, wp))
      }
    }
  }

  def wp(scope: SymTab, s: GuardedCommand, Q: Expr) : Expr = {
    val wp = wpHelper(s, Q)
    TypeCheck.tc(scope, wp, true)
    check("wp", wp.t == BoolType(), "wp is not boolean expr", wp)
    wp
  }

  def tc(scope: SymTab, s: GuardedCommand): Unit = {
    s match {
      case Skip()            => {

      }
      case Update(loc, e)    => {
        val t1 = TypeCheck.tc(scope, loc, true)
        val t2 = TypeCheck.tc(scope, e, true)
        check("wp", t1 == t2, s"update has incompatible types: ${pp(t1)} and ${pp(t2)}", loc)
      }
      case GuardedStmt(p, s) => {
        val t = TypeCheck.tc(scope, p, true)
        check("wp", t == BoolType(), "statement board is not boolean expr", p)
        tc(scope, s)
      }
      case Do(conds)         => {
        for (c <- conds) {
          tc(scope, c)
        }
      }
    }
  }
}
