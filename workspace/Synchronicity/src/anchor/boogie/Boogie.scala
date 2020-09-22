
package anchor.boogie

import java.io._

import acme.scala.Util._
import anchor.util.Errors

import scala.sys.process._


case class BoogieError(val file: String,
                       val line : Int,
                       val column : Int,
                       val message : String,
                       val trace: Option[List[BufPos]] = None,
                       val model : Option[BoogieMap] = None) {
  override def toString() : String = {
    s"${file}(${line},${column}): ${message}"
  }
}

case class BoogieResult(val errors : List[BoogieError], val existentialValues : Option[Map[String, Boolean]])

class Boogie(val boogieCmd : String = "boogie.exe", val boogieParams : String = "") {

  // z3 config to match old boogie config.
  val z3Flags = "" +
  " -proverOpt:C:AUTO_CONFIG=false" +
  " -proverOpt:C:pp.bv_literals=false" +
  " -proverOpt:C:MODEL.V2=true" +
  " -proverOpt:C:smt.PHASE_SELECTION=0" +
  " -proverOpt:C:smt.RESTART_STRATEGY=0" +
  " -proverOpt:C:smt.RESTART_FACTOR=1.5" +
  " -proverOpt:C:smt.ARITH.RANDOM_INITIAL_VALUE=true" +
  " -proverOpt:C:smt.CASE_SPLIT=3" +
  " -proverOpt:C:smt.DELAY_UNITS=true" +
  " -proverOpt:C:NNF.SK_HACK=true" +
  " -proverOpt:C:smt.MBQI=false" +
  " -proverOpt:C:smt.QI.EAGER_THRESHOLD=100" +
  " -proverOpt:C:TYPE_CHECK=true" +
  " -proverOpt:C:smt.BV.REFLECT=true" +
  " -proverOpt:C:MODEL_COMPRESS=false"

  private val summaryRegExp = raw"Boogie program verifier finished with \d+ verified, (\d+) error.*".r
  private val parseSummaryRegExp = raw"(\d+) .* errors detected in .*".r
  private val errorRegExp = raw"([^(]*)\((\d+),(\d+)\): (.*)".r
  private val timeOutRegExp = raw"([^(]*)\((\d+),(\d+)\): (Verification of .*)".r
  private val traceRegExp = raw"    ([^(]*)\((\d+),(\d+)\): (.*)".r

  private val modelStart = raw"\*\*\* MODEL".r
  private val modelEnd = raw"\*\*\* END_MODEL".r

  private val houdiniStart = raw"Assignment computed by Houdini:".r
  private val houdiniTrueLine = raw"(\w+) = True".r
  private val houdiniFalseLine = raw"(\w+) = False".r

  def makeErrorFromLine(text : String) : Option[BoogieError] = {
    text match {
      case errorRegExp(file, line, column, message) => Some(new BoogieError(file, Integer.parseInt(line), Integer.parseInt(column), message))
      case _ => None
    }
  }

  def processOutput(out : Array[String]) : BoogieResult = {
    var errors : List[BoogieError] = Nil
    var existentials : Option[Map[String,Boolean]] = None
    var i = 0;
    var pendingModels : List[BoogieMap] = Nil
    while (i < out.length) {
      val text = out(i)
      text match {
        case modelStart()                               => {
          var s = List[String]()
          s = s :+ text
          var ok = true
          while (ok) {
            i += 1
            s = s :+ out(i)
            out(i) match {
              case modelEnd() => {
                ok = false
              }
              case _          => {}
            }
          }
          pendingModels = pendingModels :+ BoogieModel.parse(s.mkString("", "\n", "\n"))
        }
        case timeOutRegExp(file, line, column, message) => {
          println(s"${file}(${line},${column}): ${message}")
          i += 1
        }
        case errorRegExp(file, line, column, message)   => {
          i += 1
          val model = pendingModels match {
            case Nil => None
            case (head::tail)    => {
              pendingModels = tail
              Some(head)
            }
          }

          if (out(i) == "Execution trace:") {
            i += 1
            var trace = List[BufPos]()
            var ok = true
            while (ok) {
              out(i) match {
                case traceRegExp(file, line, column, label) => {
                  i += 1
                  trace = trace :+ new BufPos(Integer.parseInt(line), Integer.parseInt(column))
                }
                case _                                      => ok = false
              }
            }
            errors = errors :+ BoogieError(file, Integer.parseInt(line), Integer.parseInt(column), message, Some(trace), model)
          } else {
            errors = errors :+ BoogieError(file, Integer.parseInt(line), Integer.parseInt(column), message, None, model)
          }
        }
        case houdiniStart()                             => {
          i += 1
          existentials = Some(Map[String, Boolean]())
          var ok = true
          while (ok) {
            out(i) match {
              case houdiniTrueLine(x)  => {
                existentials = Some(existentials.get + {
                  x -> true
                })
                i += 1
              }
              case houdiniFalseLine(x) => {
                existentials = Some(existentials.get + {
                  x -> false
                })
                i += 1
              }
              case _                   => ok = false

            }
          }
          pendingModels = Nil
          errors = Nil  // all prior errors are for Houdini runs -- get the last set now.
        }
        case parseSummaryRegExp(_) => {
          if (out(i).contains("parse error") || out(i).contains("type checking error") || out(i).contains("name resolution"))
            Errors.warn("Boogie", "Bad boogie File")
          i += 1
        }
        case _ => i += 1
      }
    }
    assert(pendingModels == Nil, "More models than errors!")
    BoogieResult(errors, existentials)
  }

  private def totalErrors(line : String) : Int = {
    line match {
      case summaryRegExp(n) => Integer.parseInt(n)
      case parseSummaryRegExp(n) => Integer.parseInt(n)
    }
  }

  def verify(boogieFile: String, program : String) : BoogieResult = {
    val pw = new PrintWriter(new File(boogieFile))
    pw.write(program)
    pw.close()

    val output = new PrintWriter(new File(s"$boogieFile.out"))

    val (exitCode, _) = timeAndRecord(s"Running Boogie on: $boogieFile") {
      val cmd = s"$boogieCmd $boogieParams $z3Flags $boogieFile"
      log(cmd)
      cmd ! ProcessLogger(
        line => { output.println(line); if (line.startsWith("Verifying") || (line.contains("obligation"))) { log(line) } },
        line => log(line))
    }

    output.close()

    if (exitCode == 0) {
      val source = scala.io.Source.fromFile(s"$boogieFile.out")
      val out = source.getLines().toArray
      processOutput(out)
    } else {
      Errors.fail("Boogie", s"Boogie exited with code $exitCode.  See $boogieFile.out.")
    }
  }
}

/*
a.bpl(3273,3): Error BP5001: This assertion might not hold.
Execution trace:
    a.bpl(2494,6): anon0
    a.bpl(2508,3): anon222_Then
    a.bpl(2512,11): anon3
    a.bpl(2526,3): anon223_Then
    a.bpl(2530,11): anon6
    a.bpl(2537,3): anon224_Then
    a.bpl(2538,4): anon225_Then
    a.bpl(2543,4): anon226_Then
    a.bpl(2548,4): anon227_Then
    a.bpl(2552,12): anon16
    a.bpl(2568,2): exit_13_bottom
    a.bpl(2574,3): anon228_Then
    a.bpl(2578,11): anon20
    a.bpl(2588,3): anon229_Else
    a.bpl(2589,4): anon230_Then
    a.bpl(2600,5): anon231_Then
    a.bpl(2604,13): anon26
    a.bpl(2611,5): anon232_Then
    a.bpl(2614,6): anon233_Else
    a.bpl(2619,6): anon234_Else
    a.bpl(2624,6): anon235_Else
    a.bpl(2626,14): anon36
    a.bpl(2648,4): anon236_Else
    a.bpl(2652,2): anon41
    a.bpl(2657,3): anon237_Then
    a.bpl(2661,2): anon44
    a.bpl(2670,3): anon238_Then
    a.bpl(2674,11): anon47
    a.bpl(2688,3): anon239_Then
    a.bpl(2692,11): anon50
    a.bpl(2699,3): anon240_Then
    a.bpl(2700,4): anon241_Then
    a.bpl(2705,4): anon242_Then
    a.bpl(2710,4): anon243_Then
    a.bpl(2714,12): anon60
    a.bpl(2747,2): anon244_Else
    a.bpl(2751,3): anon246_Then
    a.bpl(2755,11): anon70
    a.bpl(2760,2): anon247_Else
    a.bpl(3150,4): anon286_Then
    a.bpl(3154,12): anon178
    a.bpl(3239,3): anon287_Else
    a.bpl(3244,4): anon297_Then
    a.bpl(3248,12): anon208
    a.bpl(3254,4): anon298_Then
    a.bpl(3259,4): anon299_Then
    a.bpl(3264,4): anon300_Then
    a.bpl(3268,12): anon217


 */