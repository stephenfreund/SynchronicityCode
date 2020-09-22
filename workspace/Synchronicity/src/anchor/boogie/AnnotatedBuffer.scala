package anchor.boogie

import scala.collection.{mutable, _}


abstract class Annotation {
  var start : BufPos = null
  var end: BufPos = null

  def contains(x : BufPos): Boolean = {
    start <= x && x <= end
  }
}

private abstract class Item
private case class Push() extends Item
private case class Pop() extends Item
private case class Separator(val sep: String) extends Item
private case class Text(val text: String) extends Item
private case class Begin(val a : Annotation) extends Item
private case class End() extends Item

class AnnotatedBuffer(val separator: String = "\n") {


  private var items = mutable.MutableList[Item]()

	def push() {
	  items += Push()
	}

	def pop() {    
	  items += Pop()
	}

  def begin(a : Annotation): Unit = {
    items += Begin(a)
  }

  def end() : Unit = {
    items += End()
  }

  def annotate[T](a : Annotation)(f: => T): T = {
    begin(a)
    val t = f
    end()
    t
  }

  private def append(line : String) : Unit = {
    if (line.trim().startsWith("@")) {
      push()
      append(line.trim().substring(1).trim())
      pop()
    } else {
      items += Text(line.trim())
    }
  }


	private def sep() = {    
	  items += Separator(separator)
	}

  def emit(other : AnnotatedBuffer) : Unit = {
    items += Separator(separator)
    items ++= other.items
  }

  def emit(pre: String, other1 : AnnotatedBuffer, mid : String, other2: AnnotatedBuffer, post: String) : Unit = {
    items +=  Separator(pre)
    items ++= other1.items
    items +=  Separator(mid)
    items ++= other2.items
    items +=  Separator(post)
  }

  def emit(pre: String, other : AnnotatedBuffer, post: String) : Unit = {
    	items +=  Separator(pre)
      items ++= other.items
      items +=  Separator(post)
	}

  def emit(text: String, a: Annotation) : Unit = {
    val lines : Array[String] = text.split("\n")
    sep()
    items += Begin(a)
    for (line <- lines.dropRight(1)) {
      append(line)
      sep()
    }
    append(lines.last)
    items += End()
  }

  def emit(text: String, a: Option[Annotation]) : Unit = {
    a match {
      case None => emit(text)
      case Some(a) => emit(text, a)
    }
  }

  def emit(text: String = "") : Unit = {
    for (line <- text.split("\n")) {
      if (items.length > 0) {
        sep()
      }
      append(line)
    }
  }

  def emitPreservingIndentation(text: String) : Unit = {
    for (line <- text.split("\n")) {
      if (items.length > 0) {
        sep()
      }
      items += Text(line)
    }
  }

  def emit(text: Seq[String]) : Unit = {
			for (s <- text) {
			  emit(s)
			}
	}

  def contents() : AnnotatedText = {
    var buf = new StringBuilder(1024*1024,"")
    var indentLevel = 0
    var annotations : Map[Int,List[Annotation]] = Map()
    var pendingAnnotations : List[Annotation] = Nil
    var lineLength = 0
    var curLine = 1
    def curpos() = { new BufPos(curLine, lineLength+1) }
    def append(s : String): Unit = {
      if (s == "\n") {
        curLine += 1
        lineLength = 0
      } else {
        lineLength += s.length
      }
      buf.append(s)
    }
    items += Separator("\n")
    for (item <- items) {
      item match {
        case Push() => indentLevel += 1
        case Pop()  => {
          indentLevel -= 1
          assert(indentLevel >= 0)
        }
        case Separator(s) => {
          for (c <- s) {
            if (c != '\n') {
              if (buf.last == '\n') {
                append("  " * indentLevel)
              }
            } else {
              append(" " * Math.max(0, 100 - lineLength))
              for (a <- annotations.getOrElse(curLine, Nil)) {
                append(s"       // ${a}")
              }

            }
            append(""+c)
          }
        }
        case Text(text) => {
          if (buf.length > 0 && buf.last == '\n') {
            append(" " * indentLevel)
          }
          append(text)
        }
        case Begin(a) => {
          a.start = curpos()
          a.end = curpos()
          pendingAnnotations = a :: pendingAnnotations
        }
        case End() => {
          val a = pendingAnnotations.head
          pendingAnnotations = pendingAnnotations.tail
          a.end = curpos()
          annotations = annotations + { a.start.line -> (annotations.getOrElse(a.start.line, Nil) :+ a) }
        }
      }
    }
	  new AnnotatedText(buf.toString(), annotations.values.flatten.toList)
	}

}

class BufPos(val line: Int, val column: Int) extends Ordered[BufPos] {
  override def toString() : String = s"$line.$column"

  override def compare(that: BufPos): Int = {
    if (line != that.line) {
      line - that.line
    } else {
      column - that.column
    }
  }
}


class AnnotatedText(private val text : String, private val annotations : List[Annotation]) {
  override def toString : String = text ++ table

  def table: String = {
    var result = ""
    for (i <- annotations.sortWith(_.start < _.start)) {
      result = result + s"// ${i.start}-${i.end}: ${i.toString.replace("\n", " ")}\n"
    }
    result
  }

  def annotations(line: Int, col: Int) : List[Annotation] = {
    val b = new BufPos(line, col)
    annotations.filter(_.contains(b))
  }

}


object AnnotatedBuffer {
  
  def apply(text : String): AnnotatedBuffer = {
    val buffer = new AnnotatedBuffer()
    buffer.emit(text)
    buffer
  }
  
}

