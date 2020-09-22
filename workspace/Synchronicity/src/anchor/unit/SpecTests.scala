package anchor.unit

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.matchers._
import org.scalatest._
import anchor.sink._
import anchor.util._
import anchor.tool._
import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.Assertions._
import java.io._

import anchor.tool.Anchor

abstract class AbstractSpecTests extends FunSpec with ParallelTestExecution {

  def commandLine : String

  describe("Spec Tests") {

    val files = new File((java.lang.System.getProperty("user.dir") + "/tests")).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".anchor") || name.endsWith(".sink")
    })

    def errors(lines: Array[String]) = {
      lines.filter(x => x.startsWith("///:") && !x.startsWith("///: args")).map(_.substring(4).trim).toSet
    }

    val table = Table("File", files.toSeq:_*).sorted

    new File("bpls").mkdir()

    forAll (table) { (f: File) =>

      val lines = scala.io.Source.fromFile(f).getLines().toArray
      if (lines(0) == "///: ignore") {
        it(s"${f.getName} is ignored") { }
      } else {
        val expected = errors(lines).toList.sorted
        it(s"${f.getName} should ${if (expected.size == 0) "succeed" else s"fail with ${expected.size} errors"}") {

          val args : Array[String] = if (lines(0).startsWith("///: args")) {
            lines(0).substring("///: args".length).split("[ ]+").filterNot(_.length == 0)
          } else {
            Array.empty
          }

          val (config, _) = AnchorCommandLine(commandLine.split(" ") ++ args)
          val sinker = new Anchor(config)

          val errors = sinker.verify(f).get.map(_.toString().trim).toSet.toList.sorted
          if (expected != errors) {
            val extra = errors.filter(!expected.contains(_))
            val missing = expected.filter(!errors.contains(_))
            fail(s"""Expected Errors:
                    |${expected.mkString("    ", "\n    ", "\n")}
                    |Extra Errors:
                    |${extra.mkString("    ", "\n    ", "\n")}
                    |Missing Errors:
                    |${missing.mkString("    ", "\n    ", "\n")}
                    |--
                    |\n""".stripMargin)
          }
        }
      }
    }
  }
}

class SpecTests extends AbstractSpecTests {
  def commandLine = "-quiet -noWarn -bpl=bpls/%SINK_NAME.bpl"
}

class StrictSpecTests extends AbstractSpecTests {
  def commandLine = "-strictStability -quiet -noWarn -bpl=bpls/%SINK_NAME.bpl"
}