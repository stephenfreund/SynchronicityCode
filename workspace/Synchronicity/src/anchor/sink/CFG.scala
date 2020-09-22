package anchor.sink

import java.io.{File, PrintWriter}

import anchor.util.Errors

import scala.language.implicitConversions
import scala.collection.mutable.{HashSet, Map, MutableList}
import scala.util.parsing.input.{NoPosition, Position}

class BasicBlock(val id: Int, val tac : TAC)  {

  val successors = new HashSet[BasicBlock]();
  val predecessors = new HashSet[BasicBlock]();

  /**
    * Add an edge from this block to successor.  This method
    * properly maintains the predecessor list as well.
    */
  def addEdge(successor : BasicBlock) = {
    successors.add(successor);
    successor.predecessors.add(this);
  }

  /**
    * Return a list of predecessors that you can iterator over.
    */
  def getPredecessors() = {
    predecessors.toList;
  }

  def getSuccessors() = {
    successors.toList;
  }

  /**
    * A printable rep for a block.  Change as you see fit.
    */
  override def toString() = {
    String.format("Block %-3s: %-20s       [pred=%-15s,succ=%-15s]",
      s"$id",
      tac,
      predecessors.map(_.id).toString,
      successors.map(_.id).toString);
  }

}



/**
  * A ControlFlowGraph manages a collection of Basic Blocks and provides
  * some basic operations on them.
  * <p>
  * Be sure to inialize the enter and exit
  * blocks with the blocks you are using for those nodes.
  * Those blocks should not contain real instructions from the method
  * being analyzed.  Instead, create two extra blocks with some special
  * instruction values to indicate that they are the enter and exit blocks.
  * See the DataFlowAnalysis documentation for more details.
  * <p>
  * This class implements the Iterable interface, so you can iterate
  * over the blocks in a graph as follows:
  * <pre>
  *   ControlFlowGraph cfg = ...;
  *   for (BasicBlock b :  cfg) {
  *     ...
  *   }
  * </pre>
  */
class ControlFlowGraph(val method : MethodDecl) extends Iterable[BasicBlock] {

  val blocks = new MutableList[BasicBlock]();
  var enter: BasicBlock = null;
  var exit: BasicBlock = null;

  {
    gen()
  }

  /**
    * Allocate a new block that holds the given instruction.
    * A unique number will be assigned to that block.
    * Returns the block.
    */
  def newBlock(tac : TAC): BasicBlock = {
    val bb = new BasicBlock(blocks.length, tac);
    blocks += bb;
    bb;
  }

  /**
    * Return the block representing enter.
    */
  def getEnter() = enter;

  /**
    * Return the block representing exit.
    */
  def getExit() = exit;

  /**
    * Set the block representing enter.
    */
  def setEnter(enter: BasicBlock) = {
    this.enter = enter;
  }

  /**
    * Set the block representing exit.
    */
  def setExit(exit: BasicBlock) = {
    this.exit = exit;
  }

  /**
    * Returns an iterator for the blocks.  The iteration
    * order is the order in which blocks were allocated.
    */
  def iterator() = blocks.iterator;

  /**
    * Returns the block for an instruction. Throws
    * an exception if not such block exists.
    */
  def getBlock(i: TAC): BasicBlock = {
    blocks.find(_.tac eq i).get
  }

  /**
    * Return a string rep for a CFG.
    */
  override def toString() = {
    blocks.mkString("\n");
  }

  /**
    * Writes a dot graph description to the file named fileName.
    * You can visually examine the graph as follows.  After generating
    * graph.dot, execute the following on the command line:
    * <pre>
    *   dot -Tpdf < graph.dot > graph.pdf
    * </pre>
    * Some escape characters and punctuation may confuse dot, in which
    * case you will need to add additional escaping commands, as I have done
    * for the few obvious special cases (", \n, etc).
    */


  def dotToFile(fileName: String) = {
    try {
      val out = new PrintWriter(new File(fileName));
      out.println("digraph G {");
      out.println("   node [shape=record];");
      for (b <- blocks) {
        out.print("B" + b.id + "[");
        out.print("label=\"{Block " + b.id + "|");
        val i = b.tac;
        out.print(s"${i.stmt.pos}:${escape(i.toString)} \\n")
        out.print("}");
        out.println("\"];");
      }
      for (b <- blocks) {
        for (bb <- b.successors) {
          out.println("B" + b.id + " -> " + "B" + bb.id + ";");
        }
      }
      out.println("}");
      out.close();
    } catch {
      case e: Throwable => throw new RuntimeException(e);
    }
  }

    def fillInPath(partial : List[Stmt]) : Option[List[Stmt]] = {
      def traverse(partial: List[Stmt], current: BasicBlock, visited: Set[(BasicBlock, BasicBlock)]): Option[List[Stmt]] = {
//        println(partial.map(_.pos))
        partial match {
          case Nil       => Some(Nil)
          case (s :: ss) => {
            current.tac match {
              case StmtTAC(x) if x eq s => traverse(ss, current.successors.head, visited).map(s :: _)  // all stmts only have 1 successorr...
              case ReturnTAC(x) if x eq s => traverse(ss, current.successors.head, visited).map(s :: _)  // all stmts only have 1 successorr...
              case GotoTAC(List(_), x) if x eq s => traverse(ss, current.successors.head, visited).map(s :: _)  // 1 target gotos have 1 successorr...
              case _                    => {
                // we find *all* paths that may fit and then take the shortest.  This
                // is to favor not going through a loop body unless we must explicitly match
                // the loop body in the trace.
                val paths = (for (succ <- current.successors) yield {
                  if (!visited.contains((current, succ))) {
                    val result = traverse(partial, succ, visited + ((current, succ)))
                    if (result != None) {
                      current.tac match {
                        case StmtTAC(x) => Some(x :: result.get)
                        case _ => result
                      }
                    } else {
                      None
                    }
                  } else {
                    None
                  }
                }).flatten
                if (paths.size > 0) {
                  return Some(paths.minBy(_.size))
                } else {
                  None
                }
              }
            }
          }
        }
      }
     this.dotToFile(acme.util.Util.makeLogFileName(s"${method.parent.name}.${method.name}.dot"))
      traverse(partial, enter, Set())
    }


  def escape(s: String) = {
    s.replace("[", "\\[").
      replace("]", "\\]")
      .replace("<", "\\<").
      replace(">", "\\>")
      .replace("{", "\\{").
      replace("}", "\\}")
      .replace("\n", "\\n").
      replace("\"", "\\\"")
  }


  def gen(): Unit = {
    val tac = TACList(method.stmt)

    val blocks : List[BasicBlock] = tac.map( x => newBlock(x) )

    def label(l : String) = { blocks.find(
      x => x.tac match {
        case LabelTAC(ll, s) if l == ll => true
        case _ => false
      }
    ).get }

    setEnter(blocks.head)
    setExit(blocks.last)

    for (i <- 0 until blocks.length) {
      val b = blocks(i)
      val succs = b.tac match {
        case StmtTAC(s) => List(blocks(i+1))
        case LabelTAC(l,_) if b eq exit => List()
        case LabelTAC(l,_) => List(blocks(i+1))
        case GotoTAC(ls,_) => ls.map(label(_))
        case ReturnTAC(_) => List(exit)
      }
      for (s <- succs) {
        b.addEdge(s)
      }
    }
  }

}

sealed abstract class TAC {
  def stmt : Stmt
}

case class StmtTAC(s : Stmt) extends TAC {
  override def toString: String = PrettyPrint.pp(s).split("\n")(0)
  override def stmt : Stmt = s
}
case class LabelTAC(l : String, s : Stmt) extends TAC {
  override def toString: String = s"$l:"
  override def stmt : Stmt = s
}

case class GotoTAC(ls : List[String], s : Stmt) extends TAC {
  override def toString: String = s"goto ${ls.mkString("", ",", "")}"
  override def stmt : Stmt = s
}

case class ReturnTAC(s: Return) extends TAC {
  override def toString: String = PrettyPrint.pp(s).split("\n")(0)
  override def stmt : Stmt = s
}



object TACList {

  implicit def c(a : TAC) : List[TAC] = List(a)

  def apply(s: Stmt): List[TAC] = {

    def gen(s : Stmt, breakLabel: String) : List[TAC] = {
      s match {
        case Block(None, body) => {
          body.flatMap(gen (_, breakLabel))
        }
        case Block(Some(l), body) => {
          body.flatMap(gen (_, breakLabel)) ++ LabelTAC(l, s)

        }
        case Sync(lock, stmt, relPos) => Errors.error("CFG", "Cannot have Sync stmt in graph -- you probably called tc before lowering", lock.pos); null
        case If(cond, trueBranch, falseBranch) => {
          val tLabel = acme.scala.Util.fresh("t")
          val fLabel = acme.scala.Util.fresh("f")
          val endLabel = acme.scala.Util.fresh("end")
          StmtTAC(s) ++
          GotoTAC(List(tLabel, fLabel), s) ++
            LabelTAC(tLabel, trueBranch) ++
            gen(trueBranch, breakLabel) ++
            GotoTAC(List(endLabel), trueBranch) ++
            LabelTAC(fLabel, falseBranch) ++
            gen(falseBranch, breakLabel) ++
            GotoTAC(List(endLabel), s) ++
            LabelTAC(endLabel, s)
        }

        case While(cond, stmt, invariants, _) => {
          val headLabel = acme.scala.Util.fresh("loopHead")
          val exitLabel = acme.scala.Util.fresh("loopExit")
          StmtTAC(s) ++
          GotoTAC(List(headLabel, exitLabel), s) ++
            LabelTAC(headLabel, s) ++
            gen(stmt, exitLabel) ++
            LabelTAC(exitLabel, s) ++
            StmtTAC(s)
        }

        case Break(None) => List(GotoTAC(List(breakLabel), s))
        case Break(Some(l)) => List(GotoTAC(List(l),s))

        case s@Return(_, _) => List(ReturnTAC(s))
        case s@LocalWrites(writes) => writes.map(StmtTAC(_))
        case _ => List(StmtTAC(s))
      }
    }

    LabelTAC(acme.scala.Util.fresh("enter"),s) ++
      gen(s, null) ++
      LabelTAC(acme.scala.Util.fresh("exit"),s)
  }

}