package anchor.util
import acme.util.Util._

import scala.sys.process._
import acme.util._
import java.io._

import scala.language.postfixOps

object System {

  def runSubProcess(cmds: String*): Unit = {
    for (cmd <- cmds) {
      cmd ! ProcessLogger(line => log(line),
                          line => log(line))

    }
  }

}

object Dot {
  def gen(namePrefix: String, dot: String): String = {
    val file = new File(Util.makeLogFileName(namePrefix + ".dot"))
    val pw = new PrintWriter(file)
    pw.write(dot)
    pw.close()
    val pdfFile = Util.makeLogFileName(s"${namePrefix}.pdf")
    assert ((file #> "dot -Tpdf" #>> new File(pdfFile) !) == 0)
    pdfFile
  }
}