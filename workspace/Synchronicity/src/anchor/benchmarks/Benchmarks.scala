package anchor.benchmarks

import java.io.File

import acme.scala.Util.log
import acme.util.option.{CommandLine, CommandLineOption}
import anchor.lang._
import anchor.util.Errors

import scala.collection.mutable
import sys.process._
import scala.language.postfixOps


sealed abstract class Item

case class Description(val s: String) extends Item

case class MidRule(val s: String = "\\midrule") extends Item

case class Benchmark(val name: String, val file: String, val testFile : Option[String] = None) extends Item {
  val baseName = new File(file).getName.dropRight(7)
}

abstract class Benchmarks {
  def benchmarkDirectory : String
  def unitTestDirectory : String
  def jarDirectory : String
  def javaDirectory : String
  def verifyExtraParams(b: Benchmark) : String = ""

  def items : List[Item]
}

object SpecBenchmarks extends Benchmarks {
  def benchmarkDirectory = "benchmarks/specs"
  def unitTestDirectory = "benchmarks/unit-tests"
  def jarDirectory = "benchmarks/jars"
  def javaDirectory = "benchmarks/java"
  override def verifyExtraParams(b: Benchmark) : String = {
    if (b.name == "Queue") {
      "-verify=MethodSpecs -maxTid=1"
    } else {
      "-verify=MethodSpecs"
    }
  }


  def items = List[Item](
    Description("Stacks"),
    Benchmark("Coarse", s"${benchmarkDirectory}/stack-coarse.anchor", Some(s"${unitTestDirectory}/stack/UnitTest.java")),
    Benchmark("Lock-free", s"${benchmarkDirectory}/stack-lock-free.anchor", Some(s"${unitTestDirectory}/stack/UnitTest.java")),

    //    MidRule(),
    Description("Linked Lists"),
    Benchmark("Coarse", s"${benchmarkDirectory}/list-coarse.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),
    Benchmark("Fine~", s"${benchmarkDirectory}/list-fine.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),

    MidRule("~\\\\[-1ex]"),
    //    Description("FastTrack: Non-preemptive"),
    Benchmark("Queue", s"${benchmarkDirectory}/queue.anchor", None),

    MidRule("~\\\\[-1ex]"),
    //    Description("FastTrack: Non-preemptive"),
    Benchmark("\\multicolumn{2}{l}{\\bf FastTrack}", s"${benchmarkDirectory}/fasttrack.anchor", Some(s"${unitTestDirectory}/fasttrack/UnitTest.java")),
  )
}

object StdBenchmarks extends Benchmarks {
  def benchmarkDirectory = "benchmarks/anchor"
  def unitTestDirectory = "benchmarks/unit-tests"
  def jarDirectory = "benchmarks/jars"
  def javaDirectory = "benchmarks/java"
  override def verifyExtraParams(b: Benchmark) : String = {
    if (b.baseName == "hashset-striped") {
      "-modAxioms"
    } else {
      ""
    }
  }


  def items = List[Item](
    Description("Stacks"),
    Benchmark("Coarse", s"${benchmarkDirectory}/stack-coarse.anchor", Some(s"${unitTestDirectory}/stack/UnitTest.java")),
    Benchmark("Lock-free", s"${benchmarkDirectory}/stack-lock-free.anchor", Some(s"${unitTestDirectory}/stack/UnitTest.java")),

    //    MidRule(),
    Description("Linked Lists"),
    Benchmark("Coarse", s"${benchmarkDirectory}/list-coarse.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),
    Benchmark("Fine", s"${benchmarkDirectory}/list-fine.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),
    Benchmark("Optimistic", s"${benchmarkDirectory}/list-optimistic.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),
    Benchmark("Lazy", s"${benchmarkDirectory}/list-lazy.anchor", Some(s"${unitTestDirectory}/list/UnitTest.java")),
    Benchmark("Lock-Free", s"${benchmarkDirectory}/list-lock-free.anchor",  Some(s"${unitTestDirectory}/list/UnitTest.java")),
    //    MidRule(),

    Description("Queues"),
    Benchmark("Coarse", s"${benchmarkDirectory}/queue-coarse.anchor", Some(s"${unitTestDirectory}/queue/UnitTest.java")),
    Benchmark("Lock-Free", s"${benchmarkDirectory}/queue-lock-free.anchor", Some(s"${unitTestDirectory}/queue/UnitTest.java")),
    Benchmark("Bounded", s"${benchmarkDirectory}/queue-bounded.anchor", Some(s"${unitTestDirectory}/queue/UnitTest.java")),

    //    MidRule(),
    Description("HashSets"),
    Benchmark("Coarse", s"${benchmarkDirectory}/hashset-coarse.anchor", Some(s"${unitTestDirectory}/hashset/UnitTest.java")),
    Benchmark("Striped", s"${benchmarkDirectory}/hashset-striped.anchor", Some(s"${unitTestDirectory}/hashset/UnitTest.java")),

    MidRule("~\\\\[-1ex]"),
    //    Description("FastTrack: Non-preemptive"),
    Benchmark("\\multicolumn{2}{l}{\\bf FastTrack}", s"${benchmarkDirectory}/fasttrack.anchor", Some(s"${unitTestDirectory}/fasttrack/UnitTest.java")),
  )
}

object CompileBenchmarks {
  var benchmarks : Benchmarks = StdBenchmarks
  def compile(baseName: String, files: List[String]) = {
    Errors.check("Compile", anchor.compile.JavaCompiler.compileToJar(files,
      javaArgs = List("-classpath", ".:./lib/hamcrest-core-1.3.jar:./lib/junit-4.12.jar"),
      outputDirectory = s"${benchmarks.javaDirectory}/${baseName}",
      javaPackage = "anchor",
      jarFile = jarForBenchmark(baseName)) == 0, "Compile Failed",
      )
  }

  def test(jarFile: String) = {
    val cmd = s"java -classpath .:./lib/hamcrest-core-1.3.jar:./lib/junit-4.12.jar:${jarFile} org.junit.runner.JUnitCore UnitTest"
    acme.scala.Util.time(cmd) {
      cmd !;
    }
  }

  def main(args: Array[String]) = {
    val commandLine = new CommandLine("CompileTable", "")
    commandLine.add(new CommandLineOption[Boolean]("specs", false, false, CommandLineOption.Kind.STABLE, "Spec Bencmarks") {
      def apply(arg: String) = {
        benchmarks = SpecBenchmarks
      }
    });
    commandLine.add(new CommandLineOption[Boolean]("help", false, false, CommandLineOption.Kind.STABLE, "Print this message.") {
      def apply(arg: String) = {
        commandLine.usage();
        System.exit(0);
      }
    });
    commandLine.apply(args)

    new File(benchmarks.jarDirectory).mkdirs()
    for (i <- benchmarks.items) {
      i match {
        case Description(s)                       =>
        case MidRule(s)                           =>
        case p@Benchmark(name, file, testFile) => {
          val jarFile = jarForBenchmark(p.baseName)
          println(s"### ${p.baseName}")
          println()
          println(s"Generating Jar File ${jarFile}")
          compile(p.baseName, file :: testFile.toList)
          println()
          println(s"Running Unit Tests")
          test(jarFile)
          println()
        }
      }
    }
  }

  private def jarForBenchmark(base: String) = {
    benchmarks.jarDirectory + "/" + base + ".jar"
  }
}

object VerifyBenchmarks  {
  var benchmarks : Benchmarks = StdBenchmarks

  def makeCommand(cmd: String, p : Benchmark): String = cmd.replace("%FILE", p.file)

  def lines(file : String) : Int = {
    val cmd = (s"./commands/count.sh ${file}")
    val result = cmd !!;
    Integer.parseInt(result.trim())
  }

  def count(file: String, r : String) : Int = {
    val cmd = s"egrep ${r} ${file}"
    val result = (cmd #| "wc -l") !!;
    Integer.parseInt(result.trim())
  }

  def counts(file: String) : Map[String, Int] = {
    val program = loadProgram(file)
    val counter = new Counter()
    counter(program)
    counter.counts.toMap.withDefault(x => 0)
  }

  def time(b: Benchmark, N: Int) : List[Double] = {
    val file = b.file
    if (N == 0) {
      val cmd = s"./commands/anchor -quiet -B=-timeLimit:600 ${benchmarks.verifyExtraParams(b)} ${file}"
      println(s"    Running with command: ${cmd}")
      val result = cmd ! ProcessLogger(
        line => println(line),
        line => println(line))
      List(0)
    } else {
      val cmd = s"./commands/time-anchor -quiet -B=-timeLimit:600 ${benchmarks.verifyExtraParams(b)} ${file}"
      println(s"    Running with command: ${cmd}")
      for (i <- 1 to 3) yield {
        val result = cmd !!;
        val t = result.trim().toDouble	
        println(s"      Warmup Run ${i}: ${t} seconds")
        if (!(t >= 0)) {
          System.err.println(s"    Could not verify ${b.file}.  Rerun with command:\n\n    ${s"anchor -B=-timeLimit:600 ${benchmarks.verifyExtraParams(b)} ${file}"}\n\n")
          return List(0)
        }
      }
      
      val nums = for (i <- 1 to N) yield {
        val result = cmd !!;
        val t = result.trim().toDouble
        println(s"      Run ${i}: ${t} seconds")
        if (!(t >= 0)) {
          System.err.println(s"    Could not verify ${b.file}.  Rerun with command:\n\n    ${s"anchor -B=-timeLimit:600 ${benchmarks.verifyExtraParams(b)} ${file}"}\n\n")
          return List(0)
        }
        t
      }
      nums.toList
    }
  }

  def verify(b: Benchmark) = {
    val file = b.file
    val cmd = s"./commands/anchor -quiet -B=-timeLimit:600 ${benchmarks.verifyExtraParams(b)} ${file}"
    val result = cmd ! ProcessLogger(
      line => println(line),
      line => println(line))
  }

  def prefix() : String = {
    if (csv.get()) {
      ""
    } else {
      ("""
       \documentclass{article}
       \""" +  """usepackage{booktabs}
       \begin{document}
       \begin{tabular}{llccccr}
       & & \multicolumn{1}{c}{Size}  & \multicolumn{1}{c}{Sync.} & & \multicolumn{1}{c}{Requires and}  & \multicolumn{1}{c}{Anchor}  \\
        \multicolumn{2}{l}{Program} & \multicolumn{1}{c}{(LOC)}  & \multicolumn{1}{c}{Specs} & \multicolumn{1}{c}{Yields} &  \multicolumn{1}{c}{Invariants} & \multicolumn{1}{c}{Time (s)} \\
        \toprule
      """).stripMargin
    }
  }

  def suffix() : String = {
    if (csv.get()) {
      ""
    } else {
      """
        |\bottomrule
        |\end{tabular}
        |\end{document}
      """.stripMargin
    }
  }


  def sep = if (csv.get()) "," else " & "
  def comment = if (csv.get()) "," else " % "

  def specCounters = List("Requires","Invariant","Decreases","Commit")

  def main(args: Array[String]) = {
    acme.util.Util.quietOption.set(true)
    val commandLine = this.commandLine()

    commandLine.add(new CommandLineOption[Boolean]("specs", false, false, CommandLineOption.Kind.STABLE, "Spec Bencmarks") {
      def apply(arg: String) = {
        benchmarks = SpecBenchmarks
      }
    });

    commandLine.add(new CommandLineOption[Boolean]("list", false, false, CommandLineOption.Kind.STABLE, "Spec Bencmarks") {
      def apply(arg: String) = {
        for (b <- benchmarks.items) {
          b match {
            case b: Benchmark => println(b.baseName)
            case _            =>
          }
        }
      }
    });

    val index = commandLine(args)
    val files = {
      if (index == args.length) {
        benchmarks.items.collect({ case p:Benchmark => p.baseName })
      } else {
        args.splitAt(index)._2.toList
      }
    }

    if (N.get() == 0) {
      for (i <- benchmarks.items) {
        i match {
          case Description(s)                                                        =>
          case MidRule(s)                                                            =>
          case p@Benchmark(name, file, _) if files.contains(p.baseName) => {
            this.verify(p)
          }
          case _ => { }
        }
      }
    } else {
      import java.io._
      val out = new PrintWriter(new File(if (csv.get()) "data.csv" else "data.tex" ))
      out.println(prefix())
      for (i <- benchmarks.items) {
        i match {
          case Description(s)                                           =>
            println(s"${s}")
            if (!csv.get()) out.println(s"\\multicolumn{7}{l}{\\bf ${s}}\\\\")
          case MidRule(s)                                               =>
            if (!csv.get()) out.println(s)
          case p@Benchmark(name, file, _) if files.contains(p.baseName) => {
            println(s"  ${p.name}...")
            val counts = this.counts(file)
            val times = this.time(p, N.get())
            val yieldDensity = (counts("Yield")).asInstanceOf[Double] / (counts("Sync") + counts("MemAccess"))

            def f(x: Double) = String.format("%.2f", x.asInstanceOf[Object])

            out.println(s"${if (!name.startsWith("\\")) sep else ""} ${name} ${sep} ${lines(file)} ${sep} ${counts("Coop")} ${sep} ${counts("Yield")} ${sep} ${specCounters.map(counts(_)).reduce(_ + _)} ${sep} ${f(times.reduce(_ + _) / N.get())} \\\\  ${comment} ${times.mkString(", ")}")
          }
          case _: Benchmark                                             =>
        }
      }
      out.println(suffix())
      out.close()
    }
  }

  private def loadProgram(fileName: String): Program = {
    val file = new File (fileName)
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()
    Parser.parse(lines)
  }

  val csv = CommandLine.makeBoolean("csv", false, CommandLineOption.Kind.STABLE, "Print csv.")
  val N = CommandLine.makeInteger("n", 0, CommandLineOption.Kind.STABLE, "Number of test runs.")

  private def commandLine() = {

    val commandLine = new CommandLine("Sinker", "")
    commandLine.add(new CommandLineOption[Boolean]("help", false, false, CommandLineOption.Kind.STABLE, "Print this message.") {
      def apply(arg: String) = {
        commandLine.usage();
        System.exit(0);
      }
    });

    commandLine.add(csv)
    commandLine.add(N)
    commandLine
  }
}

class Counter {
  val counts = mutable.HashMap[String ,Int]()

  var inSpec = false;

  def spec(t : => Unit) {
    val s = inSpec;
    inSpec = true;
    t
    inSpec = s
  }

  def inc(t : String, n : Int = 1): Unit = {
    counts(t) = counts.getOrElse(t, 0) + n
  }

  def apply(x: Program): Unit = {
    x.classes.map(c => apply(c))
    x.globals.map(c => apply(c))
  }

  def apply(x : ClassInvariant): Unit = {
    this(x.pred)
    x.triggers.foreach(_.foreach(this(_)))
  }

  def apply(x: ClassDecl): Unit = {
    x.arrays.map(apply(_))
    x.fields.map(apply(_))
    spec { x.invariants.map(apply(_)) }
    inc("Invariant", x.invariants.size)
    x.methods.map(apply(_))
    this(x.constructor)
  }

  def apply(x: FieldDecl): Unit = {
    apply(x.spec)
    apply(x.t)
  }

  def apply(x : MoverSpec): Unit = {
    inc("Spec")
    spec { apply(x.conditionalMover) }
    spec { x.yieldsAs.map(apply(_)) }
  }

  def apply(x : Mover): Unit = {
    inc("Mover")
  }

  private def apply(x: VarAccess) = {
  }


  def apply(x: MethodDecl): Unit = {
    inc("Requires", x.spec.requires.size)
    if (x.isPublic) {
      inc("Coop")
    }
    spec { x.spec.requires.map(apply(_)) }
    this(x.returnType)
    x.params.map(apply(_))
    apply(x.stmt)
  }

  def apply(x: ConstructorDecl): Unit = {
    inc("Requires", x.spec.requires.size)
    if (x.isPublic) {
      inc("Coop")
    }
    spec { x.spec.requires.map(apply(_)) }
    x.params.map(apply(_))
    apply(x.stmt)
  }

  def apply(x: ArrayDecl): Unit = {
    apply(x.elemType)
    apply(x.spec)
  }

  def apply(x: VarDecl): Unit = {
    apply(x.t)
  }

  def apply(x: Stmt): Unit = {
    x match {
      case VarDeclStmt(v, None)               => apply(v)
      case VarDeclStmt(v, Some(e))            => apply(v); apply(e)
      case Assign(lhs, rhs)                   => apply(lhs); apply(rhs)
      case LocalAssign(assigns)               => assigns.map(this(_))
      case Block(_, body)                     => body.map(apply(_))
      case ExprStmt(i)                        => apply(i)
      case Return(None, false)                =>
      case Return(Some(e), false)             => apply(e)
      case Return(e, true)                    =>
      case SyncBlock(lock, stmt, _)           => inc("Sync"); apply(lock); apply(stmt)
      case If(cond, t, f)                     => apply(cond); apply(t); apply(f)
      case While(cond, stmt, invs, decreases) => {
        inc("Invariant", invs.size)
        apply(cond)
        spec { invs.map(apply(_)) }
        inc("Decreases", decreases.size)
        spec { decreases.map(this(_)) }
        apply(stmt)
      };

      case Break(None)            =>
      case Break(Some(label))     =>
      case Yield(_)               => inc("Yield")
      case Commit() => inc("Commit")
      case Assume(expr)           => spec { apply(expr) }
      case Assert(expr)           => apply(expr)
      case Invariant(expr)        => inc("Invariant"); spec { apply(expr) }
      case BoogieCode(s)          =>
      case NoReductionCheck(stmt) => apply(stmt)
      case SyncStmt(_,x)             => inc("Sync"); apply(x)
    }
  }

  def apply(x: Const): Unit = {
    x match {
      case IntConst(v)             =>
      case BoolConst(v)            =>
      case NullConst()             =>
      case NullTid()               =>
      case MoverConst(m)           => apply(m)
      case EmptyCollectionConst(t) =>
    }
  }

  def apply(x : Location) : Unit = {
    x match {
      case VarAccess(name)            =>
      case FieldAccess(v, name, _)       => if (!inSpec) inc("MemAccess"); apply(v);
      case ArrayAccess(l, index)      => if (!inSpec) inc("MemAccess"); apply(l); apply(index)
    }
  }

  def apply(x: Expr): Unit = {
    x match {
      case CAS(lhs, field, expected, rhs)    => if (!inSpec) { inc("MemAccess"); inc("Sync"); }; apply(lhs); apply(expected); apply(rhs)
      case Invoke(ref, method, args, invs)   =>
        apply(ref)
        args.map(apply(_))
        inc("Invariant", invs.size)
        spec { invs.map(apply(_)) }
      case ConstExpr(c)                      => apply(c)
      case BinaryExpr(lhs, rhs, op)          => apply(lhs); apply(rhs)
      case UnaryExpr(expr, op)               => apply(expr)
      case Cond(p, tt, ff)                   => apply(p); apply(tt); apply(ff)
      case Quantified(_, decls, pred, triggers) => decls.map(apply(_)); apply(pred)
      case x: Location                       => apply(x)
      case x: PrimitiveFunction              => apply(x)
      case x: BuiltInFunctionCall     => x.arguments.map(apply(_))
      case Old(l)                     => apply(l)
    }
  }

  def apply(x: PrimitiveFunction): Unit = {
    x match {
      case Length(a)               => apply(a);
      case Lock(a)                 => apply(a);
      case Holds(e, tid)           => apply(e); apply(tid)
      case IsLocal(e, tid)         => apply(e); apply(tid)
      case IsShared(e)             => apply(e);
      case IsFresh(e)             => apply(e);
      case NextCASSucceeds(l, t)   => apply(l); apply(t)
      case Rand()                  =>
      case NextSpecStep(_)         =>
      case Alloc(name, args, invs) =>
        args.foreach(apply(_))
        inc("Invariant", invs.size)
        spec { invs.map(apply(_)) }
      case AAlloc(a, size)         => apply(size)
    }
  }

  def apply(x: Type): Unit = {
  }
}

