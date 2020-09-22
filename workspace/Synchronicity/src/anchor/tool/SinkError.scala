package anchor.tool

import anchor.sink._
import PrettyPrint._
import acme.scala.Util

import scala.util.parsing.input._
import anchor.boogie.{Annotation, BoogieError}


class SinkError(val node: Positional, message : String) extends Annotation {
  def pos = node.pos

  val id = Util.fresh()

  override def toString : String = {
    s"(${pos}): ${message}"
  }

  def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}"
  }

  def mover(ctxt: SinkErrorContext, s : Moveable): String = {
    val model = ctxt.model
    if (model != None) {
      // could be 2 for the write in a CAS operation (where the read will always be an R)
      // could be 1 if inside of a loop
      // could be 0 if in striaght-line code.
      model.get.local(s"mover${s.id}@2").getOrElse(
        model.get.local(s"mover${s.id}@1").getOrElse(
          model.get.local(s"mover${s.id}@0").getOrElse(""))).toString
    } else {
      ""
    }
  }

  def path(ctxt: SinkErrorContext, s: Moveable): String = {
    val model = ctxt.model
    if (model != None) {
      // could be 2 for the write in a CAS operation (where the read will always be an R)
      // could be 1 if inside of a loop
      // could be 0 if in striaght-line code.
      model.get.local(s"path${s.id}@2").getOrElse(
        model.get.local(s"path${s.id}@1").getOrElse(
          model.get.local(s"path${s.id}@0").getOrElse(""))).toString
    } else {
      ""
    }
  }

  def stateEncoding(ctxt: SinkErrorContext, x: ASTNode, suffix : String) = {
    val n = ctxt.method.get.spec.specSize
    ctxt.model match {
      case Some(model) => {
        (0 until n).map( i =>
          { // println(s"$$spec$$pc.$i$$${x.id}${suffix}")
          model.local(s"$$spec$$pc.$i$$${x.id}${suffix}") match {
            case Some(SinkBool(v)) => if (v) "1" else "0"
            case _    => "?"
          }}
        ).mkString("[", "", "]")
      }
      case None        => "[" + ("?"*n) + "]"
    }
  }

  def yieldSpecTransition(ctxt: SinkErrorContext, x : Stmt):String = {
    s"${stateEncoding(ctxt, x,"")} -> ${stateEncoding(ctxt, x,"_post")}"
  }

  def loopStateEncoding(ctxt: SinkErrorContext, x : Stmt):String = {
    val n = ctxt.method.get.spec.specSize
    ctxt.model match {
      case Some(model) => {
        (0 until n).map( i =>
        { // println(s"$$spec$$pc.$i$$loopHead${x.id}")
          model.local(s"$$spec$$pc.i$$loopHead${x.id}") match {
            case Some(SinkBool(v)) => if (v) "1" else "0"
            case _    => "?"
          }}
        ).mkString("[", "", "]")
      }
      case None        => "[" + ("?"*n) + "]"
    }
  }

  abstract class Formatter {
    def format(stmt: Stmt, indexInTrace: Int, len: Int) : String
  }
  class HTMLFormatter(fullModel : Option[SinkModel], val trace: List[Stmt]) extends Formatter {
    val models = fullModel match {
      case Some(value) => SinkModel.extract(value)
      case None        => Nil
    }
    // should really only extract models once...
    def fileForNodeModel(suffix: String) : Option[String] = {
      if (models.find(_.suffix == suffix) != None) {
        if (models.find(_.suffix == suffix + "_post") != None) {
          Some(s"./${id}.${suffix}_delta.pdf")
        } else {
          Some(s"./${id}.${suffix}.pdf")
        }
      } else {
        None
      }
    }
    def format(stmt: Stmt, indexInTrace: Int, len: Int) : String = {
      val str = PrettyPrint.pp(stmt).split("\n")(0)
      val firstOccurrence = trace.indexWhere(_.id == stmt.id) == indexInTrace
      val suffix = if (firstOccurrence) {
        s"${stmt.id}"
      } else {
        s"${stmt.id}_bottom"
      }

      val file = fileForNodeModel(suffix) match {
        case Some(value) =>  String.format(s"""<a target="heap_frame" href="%s">%s</a>%s""", value, str.replaceAll("<", "&lt;").replaceAll(">", "&gt;"), " " * Math.max(len - str.length(), 1))
        case None        =>  String.format(s"""%s%s""", str, " " * Math.max(len - str.length(), 1))
      }
      file
    }
  }

  class TextFormatter(fullModel : Option[SinkModel], val trace: List[Stmt]) extends Formatter {
    def format(stmt: Stmt, indexInTrace: Int, len: Int) : String = {
      val str = PrettyPrint.pp(stmt).split("\n")(0)
      String.format(s"""%s%s""", str, " " * Math.max(len - str.length(), 1))
    }
  }


  def toHTML(ctxt: SinkErrorContext) : String = {
    val trace = ctxt.trace
    if (trace != None) {
      "<pre>" + this.toLongString() + "\n  Trace:\n" + traceToString(ctxt, trace, new HTMLFormatter(ctxt.model, trace.get)) + "</pre>"
    } else {
      this.toLongString()
    }
  }

  def toLongString(ctxt: SinkErrorContext) : String = {
    val trace = ctxt.trace
    if (trace != None) {
      this.toLongString() + "\n  Trace:\n" + traceToString(ctxt, trace, new TextFormatter(ctxt.model, trace.get))
    } else {
      this.toLongString()
    }
  }

  private def traceToString(ctxt: SinkErrorContext, trace: Option[List[Stmt]], formatter: Formatter) = {
    var inlineDepth = 0
    val strs = for ((p, index) <- trace.get.zipWithIndex if !p.isInstanceOf[VarDeclStmt]) yield {
      val str = ("  " * inlineDepth) + formatter.format(p, index, 40 - 2 * inlineDepth)
      val line = String.format(s"%-6s", s"(${p.pos.line})")
      p match {
        case x@Yield(_)          => {
            String.format("     %s  === %s %s %s", line, str, this.yieldSpecTransition(ctxt, x), "" + x.id)
        }
        case Assert(_)         => {
          String.format("     %s      %s", line, str)
        }
        case While(_, _, _, _) => {
          String.format("     %s      %s %s", line, str, this.loopStateEncoding(ctxt, p))
        }
        case Commit()          => {
          String.format("     %s  [N] %s", line, str)
        }
        case x:Acquire => {
          String.format("     %s  [%1s] %s", line, pp(R()), str)
        }
        case x:Release => {
          String.format("     %s  [%1s] %s", line, pp(L()), str)
        }
        case p: Moveable       => {
          val pathStr = path(ctxt, p)
          if (pathStr == "") {
            String.format("     %s  [%1s] %s {%s}", line, mover(ctxt, p), str, "?")
          } else {
            val path = Integer.parseInt(pathStr)
            val m = p match {
              case x:Read if x.movesAs != None => pp(x.movesAs.get)
              case x:Write if x.movesAs != None => pp(x.movesAs.get)
              case _ => mover(ctxt, p)
            }
            String.format("     %s  [%1s] %s {%s}", line, m, str, pp(p.spec, path))
          }
        }
        case InlineReturn()    => {
          inlineDepth -= 1;
          ""
        }
        case x@Return(_,_) => {
          String.format("     %s  === %s %s", line, str, this.yieldSpecTransition(ctxt, x))
      }
        case _                 => {
          val result = String.format("     %6s      %s", line, str)
          p match {
            case InlineInvoke(_) => {
              inlineDepth += 1
            }
            case _               => {

            }
          }
          result
        }
      }
    }
    strs.filter(_.length != 0).mkString("", "\n", "\n")
  }
}

case class FrontEndError(val key: String, override val node: Positional, message : String) extends SinkError(node, message) {

}

case class UnknownError(boogie : BoogieError) extends SinkError(NoNode(), s"Unknown Boogie Error: ${boogie}")

case class FieldSpecError(override val node: FieldDecl, message : String) extends SinkError(node, message)
case class ArraySpecError(override val node: ArrayDecl, message : String) extends SinkError(node, message)
case class InternalError(method: MethodDecl, message : String) extends SinkError(method, message)

case class SharingError(override val node: Stmt, message : String) extends SinkError(node, message)
case class AcquireError(override val node: Stmt, message : String) extends SinkError(node, message)
case class ReleaseError(override val node: Stmt, message : String) extends SinkError(node, message)

case class ReductionError(override val node: Stmt, message : String) extends SinkError(node, message) {
  def lastMoveable(ctxt: SinkErrorContext) : Option[Moveable] = {
    val trace = ctxt.trace
    val model = ctxt.model
    trace match {
      case Some(trace) => {
        trace.filter(_.isInstanceOf[Moveable]).lastOption match {
          case Some(value) => Some(value.asInstanceOf[Moveable])
          case None        => None
        }
      }
      case None        => None
    }
  }
}

case class InvariantError(override val node: ASTNode, e: Expr, message : String) extends SinkError(e, message) {
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n  Invariant (${e.pos}):\n\n${e.pos.longString}"
  }
}

case class EnsuresError(override val node: ASTNode, e: Expr, message : String) extends SinkError(e, message) {
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n  Ensures (${e.pos}):\n\n${e.pos.longString}"
  }
}

case class MethodSpecError(override val node: ASTNode, m: MethodDecl, message : String) extends SinkError(node, message) {
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n  Method Spec for ${m.parent.name}.${m.name}: \n ${PrettyPrint.pp(m.spec)} \n\n ${m.spec.specDFA}"
  }
}

case class RequiresError(override val node: ASTNode, e: Expr, message : String) extends SinkError(e, message) {
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n  Requires (${e.pos}):\n\n${e.pos.longString}"
  }
}

case class InlineBoogieError(override val node: BoogieCode, message : String) extends SinkError(node, message)


case class AssertError(override val node: ASTNode, e: Expr, message : String) extends SinkError(node, message) {
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n  Assert (${e.pos}):\n\n${e.pos.longString}"
  }
}

case class YieldingError(override val node: HasSpec, message : String) extends SinkError(node, message) {

  override def pos = {
    node.spec.yieldsAs match {
      case Nil => node.spec.pos
      case y::ys => y.pos
    }
  }

  def yields = s"Read <= R ==> oldValue == newValue" :: node.spec.yieldsAs.map(pp(_))

  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n    Yielding Clauses:\n\n${yields.mkString("      ", " && \n      ", "\n")}"
  }

  override def toHTML(ctxt: SinkErrorContext) : String = {
    this.toLongString(ctxt) + "\n    " + String.format(s"""<a target="heap_frame" href="./%s.yield.pdf">Heap</a>""", s"${id}")
  }

}

case class YieldingTransitivityError(override val node: HasSpec, message : String) extends SinkError(node, message) {

  override def pos = {
    node.spec.yieldsAs match {
      case Nil => node.spec.pos
      case y::ys => y.pos
    }
  }

  def yields = s"Read <= R ==> oldValue == newValue" :: node.spec.yieldsAs.map(pp(_))

  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}\n\n    Yielding Clauses:\n\n${yields.mkString("      ", " && \n      ", "\n")}"
  }

  override def toHTML(ctxt: SinkErrorContext) : String = {
    this.toLongString(ctxt) + "\n    " + String.format(s"""<a target="heap_frame" href="./%s_delta.pdf">Bad Heap Updates</a>""", s"${id}")
  }

}

case class InconsistentError(override val node: MethodDecl, message : String) extends SinkError(node, message) {
  override def pos = node.returnType.pos // ugh...
  override def toLongString(): String = {
    s"(${pos}): ${message}\n\n${pos.longString}"
  }
}

case class LoopTerminationError(override val node: While, decreasing: Option[Expr], message : String) extends SinkError(node, message) {
  override def toLongString(): String = {
    if (decreasing != None) {
      s"(${pos}): ${message}\n\n${pos.longString}\n\n  Decreasing clause:\n\n${decreasing.get.pos.longString}"
    } else {
      s"(${pos}): ${message}\n\n${pos.longString}"
    }
  }
}
