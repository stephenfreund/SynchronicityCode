package anchor.util

import scala.util.parsing.input._
import anchor.sink._

case class Failure(key: String, message : String, node: Positional) extends Error(message) {
  override def toString: String = {
    if (node.pos == NoPosition) {
      s"(no position): $key Error: $message"
    } else {
      s"(${node.pos}): $key Error: $message\n\n${node.pos.longString}"
    }
  }
}

object Errors {

  def possiblyFail[T <: AnyRef](e : => T) : Option[T] = {
    try {
      Some(e)
    } catch {
      case Failure(_,_,_) => None
    }
  }

  def fail[T <: AnyRef](key: String, message : => String, node: Positional) : T = {
    throw Failure(key, message, node)
    null.asInstanceOf[T]
  }

  def fail[T <: AnyRef](key: String, message : => String) : T = {
    fail(key, message, NoNode())
  }

  def fail[T <: AnyRef](key: String, message : => String, pos: Position) : T = {
    fail(key, message, NoNode(pos))
  }

  def error(key: String, message : => String, node: Positional) {
    throw Failure(key, message, node)
  }

  def error(key: String, message : => String) {
    error(key, message, NoNode())
  }

  def error(key: String, message : => String, pos: Position) {
    error(key, message, NoNode(pos))
  }


  def check(key: String, test : => Boolean, message : => String, node: Positional) {
    if (!test) {
      error(key, message, node)
    }
  }

  def check(key: String, test : => Boolean, message : => String) {
    check(key, test, message, NoNode())
  }

  var noWarn : Boolean = false

  def warn(key: String, message : => String, node: Positional) {
    if (!noWarn) {
      println(s"(${node.pos}): $key Warning: $message\n\n${node.pos.longString}")
    }
  }

  def warn(key: String, message : => String) {
    warn(key, message, NoNode())
  }

  def warn(key: String, message : => String, pos: Position) {
    warn(key, message, NoNode(pos))
  }

  def warn(key: String = "", test : => Boolean, message : => String, node: Positional) {
    if (!test) {
      warn(key, message, node)
    }
  }


}
