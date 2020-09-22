package anchor.sink

import java.io.{File, PrintWriter}

import anchor.boogie.AnnotatedBuffer
import PrettyPrint._
import scala.sys.process._
import scala.language.postfixOps

object SpecPrinter {

  def dot(cm : Spec, fileBaseName: String, path : Option[Int]): Unit = {

    val buffer = AnnotatedBuffer

    def gen(c: Expr, mark: Boolean, path: Option[Int]): (String, String) = {
      val node = acme.scala.Util.fresh("node")
      val attr = if (mark) "style=filled, bgcolor=\"#bbbbbb\"" else "bgcolor=\"#ffffff\""

      c match {
        case Cond(p, t, f) => {
          val (lmark, rmark, subpath) = path match {
            case Some(n) => (n % 2 == 1, n % 2 == 0, Some(n / 2))
            case None    => (false, false, None)
          }
          val (left, ld) = gen(t, lmark, subpath)
          val (right, rd) = gen(f, rmark, subpath)
          (node,
            s"""
               |    ${node}[label="${pp(p)}",${attr}];
               |${ld}
               |${rd}
               |    $node -> ${left}[label="Y"];
               |    $node -> ${right}[label="N"];
          """.stripMargin)
          }
        case m       =>
          (node, s"""      ${node}[label="${pp(m)}",${attr}];""")
      }
    }

    val text = s"""
                      |digraph G {
                      |  ${gen(cm.conditionalMover, path != None, path)._2}
                      |}
      """.stripMargin

    val file = new File(fileBaseName + ".dot")
    val output = new PrintWriter(file)
    output.println(text)
    output.close()
    file #> "dot -Tpdf" #>> new File(s"${fileBaseName}.pdf") !


  }

  def dot(fd : FieldDecl, fileBaseName: String, path : Option[Int]) : Unit = {
    dot(fd.spec, fileBaseName, path)
  }

  def dot(ad: ArrayDecl, fileBaseName: String, path : Option[Int]) : Unit = {
    dot(ad.spec, fileBaseName, path)
  }

  def dot(program : Program): Unit = {
    for (c <- program.classes) {
      for (fd <- c.fields) {
        dot(fd, s"${fd.parent.name}.${fd.name}", None)
      }
      for (ad <- c.arrays) {
        dot(ad, s"${ad.parent.name}.${ad.name}", None)
      }
    }
  }


}
