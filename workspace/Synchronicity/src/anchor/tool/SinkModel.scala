package anchor.tool

import anchor.boogie._
import anchor.sink._

sealed abstract class Phase
case class PreCommit() extends Phase {
  override def toString: String = "Pre"
}
case class PostCommit() extends Phase {
  override def toString: String = "Post"
}
case class PhaseError() extends Phase {
  override def toString: String = "Error"
}

sealed abstract class State
case class Fresh() extends State {
  override def toString: String = "Fresh"
}
case class Shared() extends State {
  override def toString: String = "Shared"
}
case class Unk() extends State {
  override def toString: String = "?"
}
case class Local(val tid: Int) extends State {
  override def toString: String = s"Local(${tid})"
}


sealed abstract class SinkValue
case class SinkInt(val v : Int) extends SinkValue {
  override def toString: String = s"${v}"
}
case class SinkBool(val v : Boolean) extends SinkValue {
  override def toString: String = s"${v}"
}

sealed abstract class SinkAddress extends SinkValue
case class SinkNull(val t : String) extends SinkAddress {
  override def toString: String = "null"
}
case class SinkRef(val id : Int, val t : String) extends SinkAddress {
  override def toString: String = s"${t}.${id}"
}

case class SinkMover(val v : Mover) extends SinkValue {
  override def toString: String = PrettyPrint.pp(v)
}
case class SinkPhase(val v : Phase) extends SinkValue {
  override def toString: String = v.toString
}
case class SinkState(val v : State) extends SinkValue {
  override def toString: String = v.toString
}

case class SinkMap(val map: Map[SinkValue, SinkValue], val elseValue : SinkValue) extends SinkValue {
  override def toString: String = (map.map(x => s"${x._1}:${x._2}")).mkString("[", ",", s",*:${elseValue}]")
}

case class SinkSeq(val terms : List[SinkValue]) extends SinkValue {
  override def toString : String = terms.mkString("[", ",", "]")
}

case class SinkUnk(val boogieTerm : BoogieExpr) extends SinkValue {
  override def toString: String = s"Unk: ${boogieTerm}"
}

class SinkModel(val boogie: BoogieMap, val suffix : String) {

  val refPattern = raw"T@([A-Za-z.$$_][A-Za-z._$$0-9]*)!val!(\d+)".r

  val special: Map[BoogieWord, SinkValue] = List[(BoogieWord, SinkValue)](
    BoogieWord("NULL") -> SinkUnk(BoogieNone()),
    boogieConstantForString("PreCommit") -> SinkPhase(PreCommit()),
    boogieConstantForString("PostCommit") -> SinkPhase(PostCommit()),
    boogieConstantForString("PhaseError") -> SinkPhase(PhaseError()),
    boogieConstantForString("_B") -> SinkMover(B()),
    boogieConstantForString("_L") -> SinkMover(L()),
    boogieConstantForString("_R") -> SinkMover(R()),
    boogieConstantForString("_N") -> SinkMover(N()),
    boogieConstantForString("_E") -> SinkMover(E())).toMap

  private def boogieConstantForString(s : String) = {
    boogie(BoogieWord(s)).asInstanceOf[BoogieWord]
  }

  private def nullFor(c: String) = {
    val BoogieWord(v) = boogie.getOrElse(BoogieWord(s"${c}.null"), BoogieWord(s"${c}.null"))
    v
  }

  private def ref(v: String) = {
    v match {
      case refPattern(c, v) => Some(SinkRef(Integer.parseInt(v), c))
      case _                => None
    }
  }

  def local(x: String): Option[SinkValue] = {
    boogie.get(BoogieWord(x + suffix)).map(convert(_))
  }

  def localInt(x: String): Option[Int] = {
    boogie.get(BoogieWord(x + suffix)).map(convert(_)).map(_.asInstanceOf[SinkInt].v)
  }

  def objects(): Set[SinkRef] = {
    def objects(b: BoogieExpr): Set[SinkRef] = {
      b match {
        case BoogieNone()            => Set()
        case x@BoogieWord(v) if special.contains(x) || v.contains("Phase") => Set()
        case BoogieWord(v)           => {
          ref(v) match {
            case Some(value) => Set(value)
            case None        => Set()
          }
        }
        case BoogieInt(v)            => Set()
        case BoogieBool(v)           => Set()
        case BoogieAsArray(v)        => Set()
        case BoogieMap(v, elseValue) => v.values.flatMap(objects(_)).toSet ++ v.keys.flatMap(objects(_)).toSet
        case BoogieLambda(x, v)      => objects(v)
        case BoogieIf(e, t, f)       => objects(e) ++ objects(t) ++ objects(f)
        case BoogieEq(e1, e2)        => objects(e1) ++ objects(e2)
        case BoogieApp(terms)        => terms.flatMap(objects(_)).toSet
        case BoogieSeq(terms)        => terms.flatMap(objects(_)).toSet
      }
    }
    val s = boogie.filter(s => s._1 match {
      case BoogieWord(v) => v.endsWith(suffix) || v.contains("k!")
      case _ => false
    }).values.flatMap(objects(_)).toSet
 //   println(s)
    val objs = List("Stack.0", "Node.9", "Node.0")
  //  s.filter(p => { objs.contains(p.t + "." + p.id) } )
    s
  }


  private def convert(b: BoogieExpr): SinkValue = {
    b match {
      case BoogieNone()                             => SinkUnk(b)
      case x@BoogieWord(v)                          => {
        this.special.get(x) match {
          case Some(value) => value
          case None        => {
            ref(v) match {
              case Some(SinkRef(n, c)) => {
                if (boogie.get(BoogieWord(s"${c}.null")) == Some(b)) {
                  SinkNull(c)
                } else {
                  SinkRef(n, c)
                }
              }
              case None                => SinkUnk(b)
            }
          }
        }
      }

      case BoogieInt(v)  => SinkInt(v)
      case BoogieBool(v) => SinkBool(v)

      case BoogieAsArray(v)        => {
        val bMap = boogie(BoogieWord(v)).asInstanceOf[BoogieMap]
        SinkMap(bMap.v.map(x => (convert(x._1), convert(x._2))), convert(bMap.elseValue))
      }
      case BoogiePair(a,b)        => ???
      case BoogieMap(v, elseValue) => ???
      case BoogieLambda(x, v)      => ???
      case BoogieSeq(terms) => SinkSeq(terms.map(convert(_)))

      case BoogieIf(e, t, f) => ???
      case BoogieEq(e1, e2)  => ???
      case BoogieApp(terms)  => terms match {
        case BoogieWord("NULL") :: Nil                                      => SinkUnk(b)
        case BoogieWord("FRESH") :: Nil                                      => SinkState(Fresh())
        case BoogieWord("SHARED") :: Nil                                     => SinkState(Shared())
        case BoogieWord("LOCAL") :: BoogieInt(n) :: Nil                      => SinkState(Local(n))
        case BoogieWord("moverPath") :: BoogieWord(v) :: BoogieInt(n) :: Nil => SinkUnk(b)
        case _                                                               => SinkUnk(b)
      }
    }
  }

  def field(p: SinkRef, f: String): SinkValue = {
    val mapName = BoogieWord(s"${p.t}.${f}${suffix}")
    val objectName = BoogieWord(s"T@${p.t}!val!${p.id}")
    convert(BoogieModel.eval(boogie, BoogieApp(List(boogie(mapName), objectName))))
  }

  def element(p: SinkRef, i: SinkInt): SinkValue = {
    val mapName = BoogieWord(s"${p.t}._elems${suffix}")
    val objectName = BoogieWord(s"T@${p.t}!val!${p.id}")
    val elems = BoogieModel.eval(boogie, BoogieApp(List(boogie(mapName), objectName)))
    convert(BoogieModel.eval(boogie, BoogieApp(List(elems, BoogieInt(i.v)))))
  }

  def elements(p: SinkRef): List[(SinkInt, SinkInt, SinkValue)] = {
    val len = length(p).v
    var result : List[(SinkInt, SinkInt, SinkValue)] = Nil
    var i = 0
    while (i < len) {
      val value = element(p, SinkInt(i))
      result = result match {
        case (lo, hi, v) :: rest if value == v && hi.v + 1 == i => (lo, SinkInt(i), value) :: rest
        case rest                         => (SinkInt(i), SinkInt(i), value) :: result
      }
      i += 1
    }
    return result.reverse
  }

  def length(p: SinkRef): SinkInt = {
    field(p, "_length").asInstanceOf[SinkInt]
  }

  def objects(t: String): Set[SinkRef] = {
    objects().filter(_.t == t)
  }

  def locals(prefix : String): Map[String, SinkValue] = {
    boogie.filter(s => s._1 match {
      case BoogieWord(v) => v.startsWith(prefix) && v.endsWith(suffix) && !v.contains(".")
      case _ => false
    }).map(e => { (e._1.asInstanceOf[BoogieWord].v.dropRight(suffix.length), convert(e._2)) })
  }

  private def locals(): Map[String, SinkValue] = {
    boogie.filter(s => s._1 match {
      case BoogieWord(v) => v.endsWith(suffix) && (!v.contains(".") || v.contains("$spec$"))
      case _ => false
    }).map(e => { (e._1.asInstanceOf[BoogieWord].v.dropRight(suffix.length), convert(e._2)) })
  }

  def pp(v: SinkValue) = v.toString

  def dot(program: Program): String = {
    def dotObjectNode(ref: SinkRef) = {
      val fields = program.scope.resolveClass(ref.t).get.fields.map(fd => (fd.name, pp(this.field(ref, fd.name))))
      s"""
         |  "${pp(ref)}" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td port = '_base' colspan='2'><b>${pp(ref)}</b></td></tr>
         |      <tr><td>State</td><td>${pp(field(ref, "_state"))}</td></tr>
         |      ${fields.map(f => s"""<tr><td align='left'>${f._1}</td><td align='center'  port='${f._1}'>${f._2}</td></tr>""").mkString("")}
         |     </table>
         |    >
         |  ];
      """.stripMargin
    }
    def dotArrayNode(ref: SinkRef) = {
      val len = length(ref)
      val elems = elements(ref).map({
        case (lo,hi,v) if lo == hi => s"""<tr><td port='f${lo}'>${lo}</td><td>${v}</td></tr>"""
        case (lo,hi,v) => s"""<tr><td port='f${lo}'>${lo}-${hi}</td><td>${v}</td></tr>"""
      })
      s"""
         |  "${pp(ref)}" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td port = '_base' colspan='2'><b>${pp(ref)}</b></td></tr>
         |      <tr><td>Length</td><td>${pp(field(ref, "_length"))}</td></tr>
         |      <tr><td>State</td><td>${pp(field(ref, "_state"))}</td></tr>
         |      ${elems.mkString}
         |     </table>
         |    >
         |  ];
      """.
        stripMargin
    }

    def dotNode(ref: SinkRef): String = {
      if (isVisibleLocalOrGlobal(ref)) {
        if (ref.t.startsWith("Array.")) {
          dotArrayNode(ref)
        } else {
          dotObjectNode(ref)
        }
      } else {
        ""
      }
    }

    def dotNodeEdges(ref: SinkRef) = {
      val refFields = program.scope.resolveClass(ref.t).get.fields.filter(fd => Type.isHeapReference(fd.t))
      refFields.map(fd => {
        val value = this.field(ref, fd.name)
        if (value.isInstanceOf[SinkRef]) {
          s"""
             |  "${pp(ref)}":"${fd.name}" -> "${pp(value)}":_base;
           """.stripMargin
        } else {
          ""
        }
      }).mkString("\n")
    }

    def dotArrayEdges(ref: SinkRef): String = {
        elements(ref).map({
          case (lo, hi, value) =>
            if (value.isInstanceOf[SinkRef]) {
              s"""
                 |  "${pp(ref)}":f${lo} -> "${pp(value)}":_base;
             """.stripMargin
            } else {
              ""
            }
        }).mkString("\n")
    }

    def dotEdges(ref: SinkRef): String = {
      if (isVisibleLocalOrGlobal(ref)) {
        if (ref.t.startsWith("Array.")) {
          dotArrayEdges(ref)
        } else {
          dotNodeEdges(ref)
        }
      } else {
        ""
      }
    }

    def localNodeCompare(a : (String,String), b: (String,String)) : Boolean = {
      (a._1, b._1) match {
        case ("tid", _) => true
        case ("this", "tid") => false
        case (_, "this") => false
        case (s, t) if (s.startsWith("tmp") && t.startsWith("tmp")) => {
          try {
            val si = Integer.parseInt(s.substring(3))
            val ti = Integer.parseInt(t.substring(3))
            si < ti
          } catch {
            case _ : Throwable => s < t
          }
        }
        case (s, t) => s < t
      }
    }

    def localNodes(): String = {
      val locals = this.locals().map(x => (x._1, pp(x._2)))
      s"""
         |  "LOCALS" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td colspan='2'><b>LOCALS</b></td></tr>
         |      ${locals.toList.sortWith(localNodeCompare).map(f => s"""<tr><td align='left'>${f._1}</td><td port='${f._1}'>${f._2}</td></tr>""").mkString("")}
         |     </table>
         |    >
         |  ];
      """.stripMargin
    }
    def localEdges(): String = {
      val refLocals = locals.filter(x => x._2.isInstanceOf[SinkRef])
      refLocals.map(x =>
        s"""
           |  "LOCALS":"${x._1}" -> "${pp(x._2)}":_base;
         """.stripMargin
      ).mkString("\n")
    }

      """
        |digraph G {
        |  graph [
        |    rankdir = "LR"
        |  ];
        |  node [
        |    fontsize = "16"
        |    shape = "plaintext"
        |  ];
        |  edge [
        |  ];
      """.stripMargin +
        localNodes() +
        (objects().map(ref => dotNode(ref)).mkString) +
        localEdges() +
        (objects().map(ref => dotEdges(ref)).mkString) +
        """
          |}
        """.stripMargin

  }


  private def isVisibleLocalOrGlobal(ref: SinkRef) = {
    val tid = this.localInt("tid")
    val state = field(ref, "_state") match {
      case SinkState(v)   => v
      case _ => Unk()
    }
    tid match {
      case Some(v)   => state == Shared() || state == Local(v)
      case None => state != Fresh()
    }
    //for full model: true
    // true
  }

  private def isFresh(ref: SinkRef) = {
    field(ref, "_state") match {
      case SinkState(Fresh())   => true
      case _ => false
    }
  }

  def deltaAsDot(old: SinkModel, program: Program): String = {
    def tdPair(p1: String, p2: String, port: String) : String = {
      if (p1 == p2) {
        s"<td align='center'>${p1}</td><td align='center' port='$port'>${p2}</td>"
      } else {
        s"<td align='center'>${p1}</td><td align='center' port='$port'><font color='red'>${p2}</font></td>"
      }
    }

    def dotObjectNode(ref: SinkRef) = {
      val fields = program.scope.resolveClass(ref.t).get.fields.map(fd => (fd.name, pp(old.field(ref, fd.name)), pp(this.field(ref, fd.name))))
      s"""
         |  "${pp(ref)}" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td port = '_base' colspan='3'><b>${pp(ref)}</b></td></tr>
         |      <tr><td>State</td>${tdPair(pp(old.field(ref, "_state")), pp(field(ref, "_state")), "state")}</tr>
         |      ${fields.map(f => s"""<tr><td>${f._1}</td>${tdPair((if (!old.isFresh(ref)) f._2 else "-"), f._3, f._1)}</tr>""").mkString("")}
         |     </table>
         |    >
         |  ];
      """.stripMargin
    }
    def dotArrayNode(ref: SinkRef) = {
      val len = length(ref)
      val elems = elements(ref).map({
        case (lo,hi,v) if lo == hi => s"""<tr><td port='f${lo}'>${lo}</td><td>${v}</td></tr>"""
        case (lo,hi,v) => s"""<tr><td port='f${lo}'>${lo}-${hi}</td><td>${v}</td></tr>"""
      })
      val oldelems = old.elements(ref).map({
        case (lo,hi,v) if lo == hi => s"""<tr><td port='f${lo}'>${lo}</td><td>${v}</td></tr>"""
        case (lo,hi,v) => s"""<tr><td port='f${lo}'>${lo}-${hi}</td><td>${v}</td></tr>"""
      })
      s"""
         |  "${pp(ref)}" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td port = '_base' colspan='3'><b>${pp(ref)}</b></td></tr>
              | <tr><td>State</td><td>${pp(old.field(ref, "_state"))}</td><td>${pp(field(ref, "_state"))}</td></tr>
         |      <tr><td>Length</td><td>${pp(old.field(ref, "_length"))}</td><td>${pp(field(ref, "_length"))}</td></tr>
         |      <tr><td>elems</td>${tdPair(if (oldelems.size == 0) "" else s"<table border='0' cellborder='1'>${oldelems.mkString}</table>",
                                           if (elems.size == 0) "" else s"<table border='0' cellborder='1'>${elems.mkString}</table>", "")}</tr>
         |     </table>
         |    >
         |  ];
      """.
        stripMargin
    }

    def dotNode(ref: SinkRef): String = {
      if (isVisibleLocalOrGlobal(ref)) {
        if (ref.t.startsWith("Array.")) {
          dotArrayNode(ref)
        } else {
          dotObjectNode(ref)
        }
      } else {
        ""
      }
    }

    def dotNodeEdges(ref: SinkRef) = {
      val refFields = program.scope.resolveClass(ref.t).get.fields.filter(fd => Type.isHeapReference(fd.t))
      refFields.map(fd => {
        val value = this.field(ref, fd.name)
        if (value.isInstanceOf[SinkRef]) {
          s"""
             |  "${pp(ref)}":"${fd.name}" -> "${pp(value)}":_base;
           """.stripMargin
        } else {
          ""
        }
      }).mkString("\n")
    }

    def dotArrayEdges(ref: SinkRef): String = {
      elements(ref).map({
        case (lo, hi, value) =>
          if (value.isInstanceOf[SinkRef]) {
            s"""
               |  "${pp(ref)}":f${lo} -> "${pp(value)}":_base;
             """.stripMargin
          } else {
            ""
          }
      }).mkString("\n")
    }

    def dotEdges(ref: SinkRef): String = {
      if (isVisibleLocalOrGlobal(ref)) {
        if (ref.t.startsWith("Array.")) {
          dotArrayEdges(ref)
        } else {
          dotNodeEdges(ref)
        }
      } else {
        ""
      }
    }

    def localNodeCompare(a : (String,String,String), b: (String,String,String)) : Boolean = {
      (a._1, b._1) match {
        case ("tid", _) => true
        case ("this", "tid") => false
        case (_, "this") => false
        case (s, t) if (s.startsWith("tmp") && t.startsWith("tmp")) => {
          try {
            val si = Integer.parseInt(s.substring(3))
            val ti = Integer.parseInt(t.substring(3))
            si < ti
          } catch {
            case _ : Throwable => s < t
          }
        }
        case (s, t) => s < t
      }
    }


    def localNodes(): String = {
      val locals = this.locals().map(x => (x._1, pp(old.locals().find(_._1 == x._1).get._2), pp(x._2)))
      s"""
         |  "LOCALS" [
         |    label = <
         |     <table border='0' cellborder='1'>
         |      <tr><td colspan='3'><b>LOCALS</b></td></tr>
         |      ${locals.toList.sortWith(localNodeCompare).map(f => s"""<tr><td align='left'>${f._1}</td>${tdPair(f._2, f._3, f._1)}</tr>""").mkString("")}
         |     </table>
         |    >
         |  ];
      """.stripMargin
    }
    def localEdges(): String = {
      val refLocals = locals.filter(x => x._2.isInstanceOf[SinkRef])
      refLocals.map(x =>
        s"""
           |  "LOCALS":"${x._1}" -> "${pp(x._2)}":_base;
         """.stripMargin
      ).mkString("\n")
    }

    """
      |digraph G {
      |  graph [
      |    rankdir = "LR"
      |  ];
      |  node [
      |    fontsize = "16"
      |    shape = "plaintext"
      |  ];
      |  edge [
      |  ];
    """.stripMargin +
      localNodes() +
      (objects().map(ref => dotNode(ref)).mkString) +
      localEdges() +
      (objects().map(ref => dotEdges(ref)).mkString) +
      """
        |}
      """.stripMargin

  }

}

object SinkModel {
  val marker = "$recorded.state"
  def extract(model : SinkModel) : List[SinkModel] = {
    val states = model.boogie.filter(s => s._1 match {
      case BoogieWord(v) => v.contains(marker)
      case _ => false
    }).map(_._1.asInstanceOf[BoogieWord].v).map(x => x.substring(x.lastIndexOf(marker) + marker.length()))
    states.map(new SinkModel(model.boogie, _)).toList
  }
}