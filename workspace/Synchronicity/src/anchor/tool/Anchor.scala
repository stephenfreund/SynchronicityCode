package anchor.tool

import anchor.sink._

import scala.language.postfixOps
import scala.util.control._
import java.io._

import acme.scala.Util
import anchor.util.System._
import acme.scala.Util.{time, timeAndRecord}

import scala.sys.process._
import anchor.transforms._
import acme.util.option.CommandLineOption
import acme.util.option.CommandLine
import anchor.boogie._
import anchor.lang.AnchorToSink
import anchor.util.{Dot, Errors}

import scala.util.Failure


class SinkErrorContext(val program: Program,
                       val method: Option[MethodDecl],
                       val trace : Option[List[Stmt]],
                       val model : Option[SinkModel])

class SinkErrorInContext(val sinkError: SinkError,
                         val ctxt: SinkErrorContext) {

  override def toString(): String = sinkError.toString()

  def toLongString() : String = sinkError.toLongString(ctxt)

  def toHTML() : String = sinkError.toHTML(ctxt)

  def genDots(): List[String] = {
    val trace = ctxt.trace
    val model = ctxt.model
    model match {
      case None        => List.empty
      case Some(model) =>
        val models = SinkModel.extract(model)
        val dir = System.getProperty("user.dir")

        val singles = for (m <- models;
                           fileName = s"${sinkError.id}." + m.suffix;
                           dot = m.dot(ctxt.program)) yield {
          s"file://$dir/${Dot.gen(fileName, dot)}"
        }
        val deltas =
          for (pre <- models;
//               if (pre.suffix.endsWith(""));
               post <- models;
               if (post.suffix.endsWith("_post"));
               if (pre.suffix == post.suffix.dropRight(5))) yield {
            val fileName = s"${sinkError.id}.${pre.suffix}_delta"
            val dot = post.deltaAsDot(pre, ctxt.program)
            s"file://$dir/${Dot.gen(fileName, dot)}"
          }
        singles ++ deltas
    }
  }

}

object AnchorCommandLine {

  val verifyOption = CommandLine.makeString("verify", "All", CommandLineOption.Kind.STABLE, "Verification Type (None, Sanity, All, Spec, PNP, Houdini)")

  val pp = CommandLine.makeBoolean("pp", false, CommandLineOption.Kind.STABLE, "Print pretty printed source.")
  val snapShots = CommandLine.makeBoolean("snapshots", false, CommandLineOption.Kind.STABLE, "generate dots files for yield points in error traces.")
  val modAxioms = CommandLine.makeBoolean("modAxioms", false, CommandLineOption.Kind.STABLE, "include axioms for mod arithmetic.")
  val noWarn: CommandLineOption[java.lang.Boolean] = CommandLine.makeBoolean("noWarn", false, CommandLineOption.Kind.STABLE, "turn off warnings.", new Runnable() {
    def run(): Unit = {
      Errors.noWarn = noWarn.get()
    }
  })
  val home = System.getenv("ANCHOR_HOME")

  {
    assert(home != null, "ANCHOR_HOME is not defined -- did you run `source msetup`?")
  }

  val boogieCmd = CommandLine.makeString("boogie", s"mono ${home}/boogie/Binaries/Boogie.exe", CommandLineOption.Kind.STABLE, "boogie command")
  val boogieFile = CommandLine.makeString("bpl", acme.util.Util.makeLogFileName("a.bpl"), CommandLineOption.Kind.STABLE, "boogie file name -- use %SINK for sink file w/out suffix")

  val maxTid = CommandLine.makeInteger("maxTid", -1, CommandLineOption.Kind.STABLE, "Max Tid")

  val sameValueWritesCommute = CommandLine.makeBoolean("sameValueCommutes", false, CommandLineOption.Kind.STABLE, "writes/reads of same value commute.")
  val inferInvsPreservedByY = CommandLine.makeBoolean("inferInvsPreservedByY", false, CommandLineOption.Kind.STABLE, "infer which class invs are preserved by Y.")

  val boogieParams = CommandLine.makeAppendableString("B", s"-useArrayTheory -errorLimit:1 -errorTrace:1 -printModel:2 -timeLimit:600 ", " ", CommandLineOption.Kind.STABLE, "boogie command-line options")

  val strictStability = CommandLine.makeBoolean("strictStability", true, CommandLineOption.Kind.STABLE, "force strict == for stability")

  val manualHoudini = CommandLine.makeBoolean("manualHoudini", false, CommandLineOption.Kind.STABLE, "don't guess more invariants for Houdini")
  val alreadyPrepared = CommandLine.makeBoolean("alreadyPrepared", false, CommandLineOption.Kind.STABLE, "don't lower/explicit/inline.")

  val commandLine = {
    val commandLine = new CommandLine("Anchor", "")
    commandLine.add(new CommandLineOption[Boolean]("help", false, false, CommandLineOption.Kind.STABLE, "Print this message.") {
      def apply(arg: String) = {
        commandLine.usage();
        System.exit(0);
      }
    });

    commandLine.addGroup("General");
    commandLine.add(pp)

    commandLine.addGroup("Verification")
    commandLine.add(verifyOption)
    commandLine.add(sameValueWritesCommute)
    commandLine.add(inferInvsPreservedByY)
    commandLine.add(strictStability)
    commandLine.add(maxTid)
    commandLine.add(boogieFile)
    commandLine.add(manualHoudini)
    commandLine.add(alreadyPrepared)
    commandLine.add(boogieParams)

    commandLine.addGroup("Error Reporting")
    commandLine.add(noWarn)
    commandLine.add(modAxioms)
    commandLine.add(snapShots)

    commandLine
  }

  val initialConfig = Config(
    verifyOption.get(),
    sameValueWritesCommute.get(),
    strictStability.get(),
    boogieCmd.get(),
    boogieParams.get(),
    boogieFile.get(),
    manualHoudini.get(),
    alreadyPrepared.get(),
    maxTid.get(),
    pp.get(),
    snapShots.get(),
    modAxioms.get(),
    inferInvsPreservedByY.get(),
  )

  def reset() = {
    verifyOption.set(initialConfig.verifyOption)
    sameValueWritesCommute.set(initialConfig.sameValueWritesCommute)
    strictStability.set(initialConfig.strictStability)
    boogieCmd.set(initialConfig.boogieCmd)
    boogieParams.set(initialConfig.boogieParams)
    boogieFile.set(initialConfig.boogieFile)
    manualHoudini.set(initialConfig.manualHoudini)
    alreadyPrepared.set(initialConfig.alreadyPrepared)
    maxTid.set(initialConfig.maxTid)
    pp.set(initialConfig.pp)
    snapShots.set(initialConfig.snapShots)
    modAxioms.set(initialConfig.modAxioms)
    inferInvsPreservedByY.set(initialConfig.inferInvsPreservedByY)
  }

  def usage() = {
    commandLine.usage()
  }

  def options(args: Array[String]): (Config, Array[String]) = {
    val index = commandLine.apply(args);

    val config = Config(
      verifyOption.get(),
      sameValueWritesCommute.get(),
      strictStability.get(),
      boogieCmd.get(),
      boogieParams.get(),
      boogieFile.get(),
      manualHoudini.get(),
      alreadyPrepared.get(),
      maxTid.get(),
      pp.get(),
      snapShots.get(),
      modAxioms.get(),
      inferInvsPreservedByY.get()
    )

    if (index == args.length) {
      return (config, Array())
    } else {
      return (config, args.splitAt(index)._2)
    }
  }

  def apply(args: Array[String]): (Config, Array[String]) = {
    this.reset()
    this.options(args)
  }
}

case class Config(
                   val verifyOption: String,
                   val sameValueWritesCommute: Boolean,
                   val strictStability: Boolean,
                   val boogieCmd: String,
                   val boogieParams: String,
                   val boogieFile: String,
                   val manualHoudini : Boolean,
                   val alreadyPrepared : Boolean,
                   val maxTid: Int,
                   val pp : Boolean,
                   val snapShots: Boolean,
                   val modAxioms : Boolean,
                   val inferInvsPreservedByY : Boolean)

class Anchor(val config: Config) {

  import config._

  def removeDups(s: List[Stmt]): List[Stmt] = {
    s match {
      case Nil                    => Nil
      case s :: Nil               => s :: Nil
      case s :: t :: ss if s eq t => removeDups(s :: ss)
      case s :: t :: ss           => s :: removeDups(t :: ss)
    }
  }

  def recoverContext(p: Program, bpl: AnnotatedText, trace: List[BufPos]) = {
    val methodPos = trace(0)
    methodAt(bpl, methodPos) match {
      case None         => None
      case Some(method) => {
        val cfg = method.cfg
        //        cfg.dotToFile(method.name + ".dot")

        val stmts: List[Stmt] = removeDups(trace.tail.flatMap(x => stmtAt(bpl, x)))
        Some((method, cfg.fillInPath(stmts)))
      }
    }
  }

  def stmtAt(bpl: AnnotatedText, pos: BufPos): scala.Option[Stmt] = {
    bpl.annotations(pos.line, pos.column).find(_.isInstanceOf[StmtAnnotation]).map(_.asInstanceOf[StmtAnnotation].s)
  }

  def methodAt(bpl: AnnotatedText, pos: BufPos): scala.Option[MethodDecl] = {
    bpl.annotations(pos.line, pos.column).find(_.isInstanceOf[MethodAnnotation]).map(_.asInstanceOf[MethodAnnotation].x)
  }

  def stmtForError(sinkError: SinkError): Option[Stmt] = {
    sinkError.node match {
      case s: Stmt => Some(s)
      case _       => None
    }
  }

  private def bplNameFor(sinkFile: String) = {
    val (name: String, parent: String) = nameAndParentOf(sinkFile)
    config.boogieFile.replace("%SINK_NAME", name).replace("%SINK_PATH", parent)
  }

  private def extendedName(sinkFile: String, suffix: String) = {
    val (name: String, parent: String) = nameAndParentOf(sinkFile)
    name + s"-${suffix}.sink"
  }


  private def nameAndParentOf(sinkFile: String) = {
    val file = new File(sinkFile)
    val lastDot = file.getName().lastIndexOf(".")
    val name = file.getName().substring(0, lastDot)
    val parent = file.getParent() match {
      case null => ""
      case x    => x
    }
    (name, parent)
  }

  def houdini(sinkFileName: String, p: Program, preamble: String = "", options: VerifyOptions): BoogieResult = {
    // val params = boogieParams + " -contractInfer -printAssignment -trace  -errorTrace:0 -printModel:0"
    val params = boogieParams + " -contractInfer -printAssignment -errorTrace:0 -printModel:0"
    val boogie = new Boogie(boogieCmd, params);
    val bpl: AnnotatedText = genBoogie(p, preamble, options)
    boogie.verify(bplNameFor(sinkFileName), bpl.toString)
  }

  private def genBoogie(p: Program, preamble: String, options: VerifyOptions) = {
    val (bpl, _) = timeAndRecord("Generating Boogie Code") {
      new SinkPrinter(config, p, options).genBoogie(preamble)
    }
    bpl
  }

  def verify(sinkFileName: String, p: Program, preamble: String = "", options: VerifyOptions): List[SinkErrorInContext] = {
    val boogie = new Boogie(boogieCmd, boogieParams);
    val bpl: AnnotatedText = genBoogie(p, preamble, options)
    val BoogieResult(errors, _) = boogie.verify(bplNameFor(sinkFileName), bpl.toString)

    sinkErrorsFromBoogieErrors(p, bpl, errors, options.checkConsistency)
  }

  private def sinkErrorsFromBoogieErrors(p: Program, bpl: AnnotatedText, errors: List[BoogieError], includeConsistencyErrors: Boolean) = {
    // deal with inconsitency errors
    val missingConsistencyErrors =
      if (includeConsistencyErrors) {
        for (c <- p.classes;
             m <- c.methods if m.isPublic;
             expected = InconsistentError(m, "Method has inconsistent requirements.");
             if (!errors.exists(b => bpl.annotations(b.line, b.column).find(_.isInstanceOf[SinkError]) == Some(expected)))) yield {
          new SinkErrorInContext(expected, new SinkErrorContext(p, Some(m), None, None))
        }
      } else {
        Nil
      }
    val otherErrors = errors.map { error => sinkErrorFromBoogieError(p, bpl, error) }.filter(p => !p.sinkError.isInstanceOf[InconsistentError])
    if (otherErrors.find(_.sinkError.isInstanceOf[UnknownError]) != None) {
      otherErrors
    } else {
      missingConsistencyErrors ++ otherErrors
    }
  }

  private def sinkErrorFromBoogieError(p: Program, bpl: AnnotatedText, error: BoogieError) = {
    val sinkError = bpl.annotations(error.line, error.column).find(_.isInstanceOf[SinkError]) match {
      case None    => UnknownError(error)
      case Some(f) => f.asInstanceOf[SinkError]
    }

    val trace = error.trace match {
      case None    => None
      case Some(t) => recoverContext(p, bpl, t)
    }

    val sinkModel = error.model.map(new SinkModel(_, ""))

    val ctxt = (trace, stmtForError(sinkError)) match {
      case (None, _)                                        => new SinkErrorContext(p, None, None, sinkModel)
      case (Some((method, t@Some(_))), Some(stmtWithError)) => {
        if (t.get.lastOption == Some(stmtWithError)) {
          new SinkErrorContext(p, Some(method), t, sinkModel)
        } else {
          val path: List[Stmt] = t.get :+ stmtWithError
          val tailOfPath: List[Stmt] = method.cfg.fillInPath(path) match {
            case None    => Nil
            case Some(s) => s
          }
          new SinkErrorContext(p, Some(method), Some(tailOfPath), sinkModel)
        }
      }
      case (Some((method, t)), _)                           => new SinkErrorContext(p, Some(method), t, sinkModel)
    }
    new SinkErrorInContext(sinkError, ctxt)
  }

  def verify(f: File): Option[List[SinkErrorInContext]] = {
    this.loadProgram(f) match {
      case Left(value)  => verify(f.getPath, value)
      case Right(value) => Some(List(value))
    }
  }

  def useHoudini() = {
    verifyOption.contains("Houdini")
  }

  def verify(name: String, program: Program): Option[List[SinkErrorInContext]] = {
    val options = VerifyOptions(verifyOption)
    if (options == VerifyOptions.noVerify) {
      None
    } else {
      val (errors, _) = timeAndRecord(s"Verifying") {

        try {
          val (refinedProgram, preamble) : (Program, String) = {
            if (useHoudini()) {
              val (q, _) = timeAndRecord("Running Houdini") {
                runHoudini(name, program, options)
              }
              q
            } else {
              prepareProgramForVerifying(name, program, false)
            }
          }

          val result = verify(name, refinedProgram, preamble, options)

          Some(result)

        } catch {
          case anchor.util.Failure(key, message, pos) => {
            return Some(List(new SinkErrorInContext(FrontEndError(key, pos, message), new SinkErrorContext(program, None, None, None))))
          }
        }
      }
      errors
    }
  }

  private def runHoudini(name: String, program: Program, options: VerifyOptions) = {

    def doIt(sinkFileName: String, conjectured: Program, preamble: String = "", options: VerifyOptions) = {
      val BoogieResult(errors, houdiniMap) = houdini(name, conjectured, preamble, options)
      houdiniMap match {
        case Some(houdiniMap) => {
          Util.log(s"Found ${houdiniMap.count(_._2)} of ${houdiniMap.size} annotations.  ${errors.length} errors remain.")
          time(s"Generating New Version") {
            Some(FrontEnd.tc((new SubstBoolValues(houdiniMap, options.yeildsAreNoOps)) (conjectured)))
          }
        }
        case None             => {
          Util.log(s"No assignment.  ${errors.length} errors remain.")
          None
        }
      }
    }

    def printFile(program: Program, suffix: String) = {
      val out = new PrintWriter(acme.util.Util.openLogFile(extendedName(name, suffix)))
      out.println(FrontEnd.pp(program).split("\n").mkString("\n\t", "\n\t", "\n"))
      out.close()
    }


    val (conjectured, preamble) = prepareProgramForVerifying(name, program, true)

    printFile(conjectured, "conjecture")

    val result = doIt(name, conjectured, preamble, VerifyOptions.seqHoudini) match {
      case None => program
      case Some(newProgram) => {
        printFile(newProgram, "seqhoudini")
        doIt(name, newProgram, preamble, options) match {
          case None => program
          case Some(newProgram) => {
            printFile(newProgram, "houdini")
            newProgram
          }
        }
      }
    }

    (result, preamble)


  }


  private def prepareProgramForVerifying(name: String, program: Program, houdinify: Boolean): (Program, String) = {
    FrontEnd.tc(program)

    val lowered = if (config.alreadyPrepared) program else FrontEnd.lower(program)

    val explicit = if (config.alreadyPrepared) program else FrontEnd.explicit(lowered)

    val inlined = if (config.alreadyPrepared) program else FrontEnd.inline(explicit)

    val prepared = if (houdinify) FrontEnd.houdini(inlined) else inlined

    if (pp) {
      println(FrontEnd.pp(prepared))
    }

    val preamble =
      time("Creating Boogie File") {
      s"""
         | /*
         |
         | ${name}:
         |
         | AST:
         | ${FrontEnd.pp(program).split("\n").mkString("\n\t", "\n\t", "\n")}
         |
         |
         | Explicit:
         | ${FrontEnd.pp(explicit).split("\n").mkString("\n\t", "\n\t", "\n")}
         |
         |
         | Inlined:
         | ${FrontEnd.pp(inlined).split("\n").mkString("\n\t", "\n\t", "\n")}
         |
         |
         | Prepared:
         | ${FrontEnd.pp(inlined).split("\n").mkString("\n\t", "\n\t", "\n")}
         |
         | */
           """.stripMargin.replace("\t", "    ")
        }
    (prepared, preamble)
  }

  private def printErrors(errors: List[SinkErrorInContext]) = {
    for (error <- filterErrors(errors)) {
      println(error.toLongString())
      if (snapShots) {
        println(s"    Associated Heaps: ${error.genDots().map(x => s"    ${x}").mkString("\n", "\n", "\n")}")

      }
    }
  //  println()
  }

  private def filterErrors(errors: List[SinkErrorInContext]): List[SinkErrorInContext] = {
    errors
  }

  private def printErrorsAsHTML(file: File, errors: List[SinkErrorInContext]) = {
    printFrameVersion()
    val out = new PrintWriter(acme.util.Util.makeLogFileName("errors.html"))
    out.println("<html><body><pre>")
    val source = scala.io.Source.fromFile(file)
    val code = try source.mkString finally source.close()

    for ((text, line) <- code.split("\n").zipWithIndex) {
      out.println(String.format("%-8s %s", s"(${line + 1})", text))
    }
    out.println()
    out.println()
    out.println()
    for (error <- filterErrors(errors)) {
      out.println(error.toHTML())
      out.println("<br><br><br>")
    }
    out.println(s"${errors.size} Errors")
    out.println()
    out.println("</pre></body></html>")
    out.close()
  }


  def printFrameVersion() = {
    val out = new PrintWriter(acme.util.Util.makeLogFileName("errors-frames.html"))
    out.println(
      """<html>
          <frameset cols="*,*">
           <frame src="errors.html" frameborder="0">
           <frame name="heap_frame" frameborder="0">
          </frameset>
         </html>
      """)
    out.close()
  }


  private def loadSink(lines: String): Program = {
      FrontEnd.parse(lines)
  }

  private def loadAnchor(lines: String): Program = {
    val a = time("Loading Anchor code") {
      anchor.lang.FrontEnd.fe(lines)
    }

    time("Translating to Sink") {
      AnchorToSink(a)
    }
  }

  private def loadProgram(file: File): Either[Program, SinkErrorInContext] = {
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()

    // Errors.check("Input", file.getPath.endsWith(".sink"), s"Bad File Name: ${file.getPath}.")

    val (program, _) = timeAndRecord("Loading") {
      try {
        if (file.getPath.endsWith(".sink")) {
          loadSink(lines)
        } else if (file.getPath.endsWith(".anchor")) {
          loadAnchor(lines)
        } else {
          Errors.fail("Input", s"Bad File Name: ${file.getPath}.")
        }
      } catch {
        case anchor.util.Failure(key, message, pos) => {
          return Right(new SinkErrorInContext(FrontEndError(key, pos, message), new SinkErrorContext(null, None, None, None)))
        }
      }
    }
    Left(program)
  }

  def process(files: Array[String]): Int = {
    time("Main") {
      var errorCount = 0
      for (fileName <- files) {
        try {
          println(s"Processing $fileName...")
          val (errors, verifyTime) = timeAndRecord(s"Processing ${fileName}") {

            val file = new File(fileName)
            val program = this.loadProgram(file)

            program match {
              case Left(program) => {

                if (pp) {
                  println(FrontEnd.pp(program))
                }

                // loaded program
                verify(file.getPath, program) match {
                  case Some(errors) => {
                    // verify errors
                    printErrorsAsHTML(file, errors)
                    Some(errors)
                  }
                  case None         => {
                    // no errors
                    val programWithTypes = FrontEnd.tc(program)

                    None
                  }
                }
              }
              case Right(error)  => {
                // error loading program
                Some(List(error))
              }
            }
          }

          errors match {
            case Some(errors) => {
              val timeInSeconds = verifyTime / 1000000000.0
              if (errors.size == 0) {
                println(f"""Verified!  (${timeInSeconds}%2.2f seconds)""")
              } else {
                println()
                printErrors(errors)
                println(f"""${errors.size} Errors.  (${timeInSeconds}%2.2f seconds)""")
              }
              errorCount += errors.size
            }
            case None         => {

            }
          }
        } catch {
          case e: anchor.util.Failure => {
            stderr.println(e)
            errorCount += 1
          }
          case NonFatal(e)            => {
            println("Error: " + e);
            e.printStackTrace()
            errorCount += 1
          }
          case e: Throwable           => {
            println("Error: " + e);
            e.printStackTrace()
            errorCount += 1
          }
        }
      }
      errorCount
    }
  }
}

object Anchor {
  def main(args: Array[String]): Unit = {
    val (config, files) = AnchorCommandLine(args)
    if (files.size == 0) {
      acme.util.Util.error("No source files provided.");
      AnchorCommandLine.usage();
      System.exit(0);
    } else {
      val ok = new Anchor(config).process(files) == 0
      System.exit(if (ok) 0 else -1)
    }
  }
}