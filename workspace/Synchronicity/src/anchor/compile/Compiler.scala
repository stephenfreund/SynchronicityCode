package anchor.compile

import scala.language.postfixOps
import acme.util.option._
import anchor.util.{Errors, Failure}
import scala.collection.JavaConverters._


object Compiler {
  val outputDirectory = CommandLine.makeString("java", "compiled", CommandLineOption.Kind.STABLE, "output directory")
  val javaPackage = CommandLine.makeString("package", "anchor", CommandLineOption.Kind.STABLE, "package for Java classes")
  val jarFile = CommandLine.makeString("jar", "anchor.jar", CommandLineOption.Kind.STABLE, "jar file for compiled classes")
  val javaArgs = CommandLine.makeStringList("J", CommandLineOption.Kind.STABLE, "args to javac")

  val commandLine = {
    val commandLine = new CommandLine("Compiler", "")

    commandLine.add(new CommandLineOption[Boolean]("help", false, false, CommandLineOption.Kind.STABLE, "Print this message.") {
      def apply(arg: String) = {
        commandLine.usage();
        System.exit(0);
      }
    });

    commandLine.addGroup("General");

    commandLine.addGroup("Java Compilation");
    commandLine.add(jarFile)
    commandLine.add(javaPackage)
    commandLine.add(outputDirectory)
    commandLine
  }


  def usage() = {
    commandLine.usage()
  }

  def options(args: Array[String]): (Array[String]) = {
    val index = commandLine.apply(args);

    if (index == args.length) {
      return Array()
    } else {
      return args.splitAt(index)._2
    }
  }

  def main(args: Array[String]): Unit = {
    val files = options(args)
    if (files.size == 0) {
      acme.util.Util.error("No source files provided.");
      usage();
      System.exit(0);
    } else {
      try {
        var result: Int = JavaCompiler.compileToJar(files.toList, javaArgs.get().asScala.toList,outputDirectory.get(),javaPackage.get(),jarFile.get())
        System.exit(result)
      } catch {
        case e: Failure => {
          println(e.toString)
          System.exit(-1)
        }
      }
    }
  }
}
