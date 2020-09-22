package anchor.compile

import java.io._

import anchor.lang._
import anchor.util.Errors
import acme.scala.Util._

import scala.language.postfixOps
import scala.reflect.io.Directory
import scala.sys.process._

case class GeneratedClass(val className : String,
                          val body : String)

class JavaCompiler(val program : Program) {


  trait FieldAccessor {
    def decl() : String
    def read(x : Expr) : String
    def write(x : Expr, v : Expr) : String
    def cas(x : Expr, expected : Expr, v : Expr) : String
  }

  class RegularFieldAccessor(val field: FieldDecl) extends FieldAccessor {
    def decl() : String = {
      s"${if (field.isVolatile) "volatile " else ""}${pp(field.t)} ${field.name};"
    }
    def read(x : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      s"${pp(x)}.${field.name}"
    }
    def write(x : Expr, v : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      s"${pp(x)}.${field.name} = ${pp(v)}"
    }
    def cas(x : Expr, expected : Expr, v : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      Errors.fail("Java", "Cannot call CAS on non-cas-able field", x)
    }
  }

  class CASFieldAccessor(val field: FieldDecl) extends FieldAccessor {
    def decl() : String = {
      val t = field.t match {
        case IntType()        => "AtomicInteger"
        case TidType()        => "AtomicInteger"
        case BoolType()       => "AtomicBoolean"
        case ClassType(name)  => s"AtomicReference<${name}>"
        case _ => Errors.fail("Java", s"Bad atomic field type: ${field.t}", field)
      }
      s"${pp(t)} ${field.name} = new ${pp(t)}();"
    }
    def read(x : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      s"${pp(x)}.${field.name}.get()"
    }
    def write(x : Expr, v : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      s"${pp(x)}.${field.name}.set(${pp(v)})"
    }
    def cas(x : Expr, expected : Expr, v : Expr) : String = {
      assert(x.t == ClassType(field.parent.name))
      s"${pp(x)}.${field.name}.compareAndSet(${pp(expected)}, ${pp(v)})"
    }
  }

  trait Locker {
    def decl() : String
    def acq(x: Expr) : String
    def rel(x: Expr) : String
    def wait(x: Expr): String
    def notify(x: Expr): String
    def sync(x: Expr, s : Stmt) : String
  }

  class SynchronizedLocker(val c : ClassDecl) extends Locker {
    def decl() : String = ""
    def acq(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      Errors.fail("Java", "Cannot acquire object without explicit lock", x)
    }
    def rel(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      Errors.fail("Java", "Cannot acquire object without explicit lock", x)
    }
    def wait(x: Expr) : String = {
      assert (x.t == ClassType(c.name))

      s"try { ${pp(x)}.wait(); } catch(InterruptedException e) { throw new RuntimeException(e); }"
    }
    def notify(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      s"${pp(x)}.notifyAll();"
    }

    def sync(x: Expr, stmt : Stmt) : String = {
      assert(x.t == ClassType(c.name))
      s"synchronized (${pp(x)}) {${push()}${newLine()}${pp(stmt)}${pop()}${newLine()}}"
    }
  }

  class ReentrantLockLocker(val c : ClassDecl, val needsWait : Boolean) extends Locker {
    def decl() : String =
      "final ReentrantLock _lock = new ReentrantLock();" ++
        (if (needsWait) "\nfinal Condition _condition = _lock.newCondition;" else "")
    def acq(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      s"${pp(x)}._lock.lock();"
    }
    def rel(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      s"${pp(x)}._lock.unlock();"
    }
    def wait(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      assert (needsWait);
      s"${pp(x)}._condition.wait();"
    }
    def notify(x: Expr) : String = {
      assert (x.t == ClassType(c.name))
      assert (needsWait);
      s"${pp(x)}._condition.signalAll();"
    }
    def sync(x: Expr, stmt : Stmt) : String = {
      assert(x.t == ClassType(c.name))
      s"${pp(x)}._lock.lock(); try {${push()}${newLine()}${pp(stmt)}${pop()}${newLine()}} finally { ${pp(x)}._lock.unlock(); }"
    }
  }

  val accessors = {
    val fieldsWithCAS = new FindFieldWithCASOps().apply(program)
    (for (c <- program.classes; f <- c.fields) yield {
        if (fieldsWithCAS.contains(f)) {
        (f -> new CASFieldAccessor(f))
      } else {
        (f -> new RegularFieldAccessor(f))
      }
    })
  }

  val lockers = {
    val needLocks = FindReentrantLocks(program)
    val needWaits = FindWaitNotifies(program)
    (for (d <- program.classes) yield {
      if (needLocks.find(_.decl eq d) != None) {
        val needsWait = needWaits.find(_.decl eq d) != None
        (d -> new ReentrantLockLocker(d, needsWait))
      } else {
        (d -> new SynchronizedLocker(d))
      }
    })
  }

  {
    def logList[U,T](title: String, elems: List[(U,T)], p: U => String) = {
      log(s"${title}:")
      for (f <- elems.map(x => x._1)) {
        log(s"    ${p(f)}")
      }
    }

    val allFields = accessors.partition(_._2.isInstanceOf[RegularFieldAccessor])
    logList("Regular Fields", allFields._1, (f:FieldDecl) => s"${f.parent.name}.${f.name}")
    logList("CAS Fields", allFields._2, (f:FieldDecl) => s"${f.parent.name}.${f.name}")

    val allLockers = lockers.partition(_._2.isInstanceOf[SynchronizedLocker])
    logList("Classes With Implicit Lock", allLockers._1, (c:ClassDecl) => s"${c.name}")
    logList("Classes With Explicit Lock", allLockers._2, (c:ClassDecl) => s"${c.name}")
  }

  def accessorFor(f : FieldDecl) = {
    accessors.find(_._1 eq f).get._2
  }

  def lockerFor(d : ClassDecl) = {
    lockers.find(_._1 eq d).get._2
  }

  var indentLevel = 0;

  def push(): String = {
    indentLevel += 1
    ""
  }

  def pop(): String = {
    indentLevel -= 1
    ""
  }

  def newLine(): String = {
    return "\n" + ("  " * indentLevel)
  }

  def gen(): List[GeneratedClass] = {
    val classes = program.classes.map(c => GeneratedClass(c.name, pp(c)))

    def field(f : VarDecl) = s"""  public static ${pp(f.t)} ${pp(f.name)}${if (f.t.isInstanceOf[ClassType]) s" = new ${pp(f.t)}()" else ""};"""

    val globals = s"""
       |
       |public class AnchorGlobals {
       |${program.globals.map(field(_)).mkString("", "\n", "")}
       |}
     """.stripMargin

    classes :+ GeneratedClass("AnchorGlobals", globals)
  }

  def pp(x: String): String = {
    x
  }

  def pp(x: ClassDecl): String = {
    s"public class ${pp(x.name)} {${push()}${newLine()}${lockerFor(x).decl()}${newLine()}${(x.fields.filter(!_.isGhost).map(pp(_)).mkString(newLine()) ++ pp(x.constructor)::(x.methods.map(pp(_)))).mkString(newLine())}${pop()}${newLine()}}"
  }

  def pp(x: FieldDecl): String = {
      accessorFor(x).decl() + s"${newLine()}"
  }

  private def pp(x: VarAccess) = {
    if (x.scope.isGlobal(x)) {
      s"AnchorGlobals.${x.name}"
    } else {
      x.name
    }
  }

  def pp(x: MethodDecl): String = {
    val block = Block(None, x.spec.vars.map(VarDeclStmt(_,None)) :+ x.stmt)
    s"""${if (x.isPublic) "public " else ""} ${pp(x.returnType)} ${x.name}(${x.params.map(pp(_)).mkString(",")}) ${pp(block)}${newLine()}"""
  }

  def pp(x: ConstructorDecl): String = {
    val block = Block(None, x.spec.vars.map(VarDeclStmt(_,None)) :+ x.stmt)
    s"""${if (x.isPublic) "public " else ""} ${x.parent.name}(${x.params.map(pp(_)).mkString(",")}) ${pp(block)}${newLine()}"""
  }

  def pp(x: VarDecl): String = {
    s"${pp(x.t)} ${pp(x.name)}"
  }

  def pp(x: Stmt): String = {
    x match {
      case VarDeclStmt(v, None)          => s"${pp(v)};"
      case VarDeclStmt(v, Some(e))       => s"${pp(v)} = ${pp(e)};"
      case Assign(fa@FieldAccess(l,_,_), rhs) if (fa.decl.isGhost) => ""
      case Assign(fa@FieldAccess(l,_,_), rhs) => {
        val acc = accessorFor(fa.decl)
        s"${acc.write(l, rhs)};"
      }
      case Assign(lhs, rhs)              => s"${pp(lhs)} = ${pp(rhs)};"
      case LocalAssign(assigns)          => assigns.map(pp(_)).mkString(" ")
      case Block(None, body)             => s"{${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case Block(Some(n), body)          => s"${n}: {${push()}${newLine()}${body.map(pp(_)).mkString(newLine())}${pop()}${newLine()}}"
      case ExprStmt(i)                   => s"${pp(i)};"
      case Return(None, false)           => "return;"
      case Return(Some(e), false)        => s"return ${pp(e)};"
      case Return(None, true)            => ""
      case Return(Some(e), true)         => ""
      case SyncBlock(lock, stmt, _)      => {
        val locker = lockerFor(lock.t.asInstanceOf[ClassType].decl)
        locker.sync(lock, stmt)
      }
      case If(cond, t, Block(None, Nil)) => s"if (${pp(cond)}) ${pp(t)}"
      case If(cond, t, f)                => s"if (${pp(cond)}) ${pp(t)} else ${pp(f)}"
      case While(cond, stmt, invs, decs) => s"while (${pp(cond)}) ${pp(stmt)}"
      case Break(None)                   => "break;"
      case Break(Some(label))            => s"break ${label};"
      case Yield(Nil)                    => ""
      case Yield(ensures)                => ""
      case Commit()                      => ""
      case Assume(expr)                  => ""
      case Assert(expr)                  => ""
      case Invariant(expr)               => ""
      case BoogieCode(s)                 => ""
      case NoReductionCheck(stmt)        => ""
      case SyncStmt(op, x)                    => {
        val locker = lockerFor(x.t.asInstanceOf[ClassType].decl)
        op match {
          case Acquire() => locker.acq(x)
          case Release() => locker.rel(x)
          case Wait()    => locker.wait(x)
          case Notify()  => locker.notify(x)
        }

      }
    }
  }

  def specExprList(prefix: String, es: List[Expr]): String = {
    if (es == Nil) {
      ""
    } else {
      s"""${push}${es.map(i => s"${newLine()}${prefix} ${pp(i)};").mkString}${pop()}${newLine()}"""
    }
  }


  sealed abstract class Assoc()

  case class Left() extends Assoc()

  case class Right() extends Assoc()

  case class Non() extends Assoc()

  def desc(op: BinaryOp): (Int, Assoc) = {
    op match {
      case Add()     => (12, Left())
      case Sub()     => (12, Left())
      case Mul()     => (14, Left())
      case Div()     => (14, Left())
      case Mod()     => (14, Left())
      case And()     => (4, Left())
      case Or()      => (3, Left())
      case EQ()      => (8, Left())
      case NE()      => (8, Left())
      case LT()      => (9, Non())
      case GT()      => (9, Non())
      case LE()      => (9, Non())
      case GE()      => (9, Non())
      case Implies() => (1, Right())
    }
  }

  def desc(x: UnaryOp): (Int, Assoc) = {
    x match {
      case Neg()   => (14, Right())
      case Not()   => (14, Right())
      case Paren() => (17, Non())
    }
  }

  case class Result(p: String, prec: Int, assoc: Assoc)

  def ppWithParens(x: Const): Result = {
    Result(x match {
      case IntConst(v)             => v.toString()
      case BoolConst(v)            => v.toString()
      case NullConst()             => "null"
      case NullTid()               => "-1"
      case MoverConst(m)           => Errors.fail("Java", "Can't translate Mover constant", x)
      case EmptyCollectionConst(t) => t.defaultValue()
    }, 20, Non())
  }

  def ppWithParens(x: PrimitiveFunction): Result = {
    x match {
      case Length(a)       => Result(s"${pp(a)}.length", 20, Non())
      case Alloc(name, args, invs)     => Result(s"new ${pp(name)}(${args.map(pp(_)).mkString(",")})", 13, Right())
      case AAlloc(a, size) => Result(s"new ${pp(a.decl.elemType)}[${pp(size)}]", 13, Right())
      case _               => Errors.fail("Java", s"Can't translate ${x}", x)
    }
  }

  def ppWithParens(x: Location): Result = {
    Result(x match {
      case x@VarAccess(name)       => pp(x)
      case x@FieldAccess(v, name,_) if (x.decl.isGhost) => {
        Errors.fail("Compile", "Cannot access ghost in compiled context")
      }
      case x@FieldAccess(v, name,_)  => {
        val acc = accessorFor(x.decl)
        acc.read(v)
      }
      case ArrayAccess(l, index) => s"${pp(l)}[${pp(index)}]"
    }, 20, Non())
  }

  def doP(b: Boolean, s: String) = {
    if (b) {
      s"(${s})"
    } else {
      s
    }
  }

  def ppWithParens(x: Expr): Result = {
    x match {
      case ConstExpr(const)                => ppWithParens(const)
      case BinaryExpr(lhs, rhs, Implies()) => ppWithParens(BinaryExpr(AST.not(lhs), rhs, Or()))
      case BinaryExpr(lhs, rhs, op)        => {
        val l = ppWithParens(lhs)
        val r = ppWithParens(rhs)
        val (prec, assoc) = desc(op)
        val parenLeft = l.prec < prec || l.prec == prec && assoc == Right()
        val parenRight = r.prec < prec || r.prec == prec && assoc == Left()
        Result(s"""${if (parenLeft) s"(${l.p})" else l.p} ${pp(op)} ${if (parenRight) s"(${r.p})" else r.p}""", prec, assoc)
      }
      case UnaryExpr(expr, op)               => {
        val e = ppWithParens(expr)
        val (prec, assoc) = desc(op)
        Result(if (prec > e.prec) {
          s"${pp(op)}(${e.p})"
        } else {
          s"${pp(op)}${e.p}"
        }, prec, assoc)
      }
      case Invoke(ref, method, args, invs)   => {
        Result(s"${pp(ref)}.${method}(${args.map(pp(_)).mkString(",")})", 20, Non())
      }
      case location: Location                => ppWithParens(location)
      case x@CAS(lhs, field, expected, rhs)  => {
        val acc = accessorFor(x.decl)
        Result(acc.cas(lhs,expected,rhs), 20, Right())
      }
      case Quantified(_, decls, pred, triggers) => Errors.fail("Java", "Can't translate forall expression", x)
      case Cond(p, tt, ff)                   => {
        val pp = ppWithParens(p)
        val isNested = (tt match {
          case Cond(_, _, _) | UnaryExpr(Cond(_, _, _), Paren()) => true
          case _                                                 => false
        }) || (ff match {
          case Cond(_, _, _) | UnaryExpr(Cond(_, _, _), Paren()) => true
          case _                                                 => false
        })
        if (isNested) push()
        val ttt = ppWithParens(tt)
        val fff = ppWithParens(ff)
        val parenpp = pp.prec < 2 || pp.prec == 2
        val parenTT = ttt.prec < 2
        val parenFF = fff.prec < 2
        if (isNested) pop()

        def nested(s: => String) = {
          if (isNested) s else ""
        }

        Result(s"""${doP(parenpp, pp.p)}${nested(s"${newLine()}")} ? ${doP(parenTT, ttt.p)}${nested(newLine())} : ${doP(parenFF, fff.p)}""", 2, Right())
      }

      case x: PrimitiveFunction => ppWithParens(x)
      case Old(e) => Errors.fail("Java", "Can't translate old()", x)
      case BuiltInFunctionCall(name, types, arguments) => Result(s"${name}", 20, Non())

    }
  }

  def pp(x: Expr): String = {
    ppWithParens(x).p
  }

  def pp(x: BinaryOp): String = {
    x match {
      case Add()     => "+"
      case Sub()     => "-"
      case Mul()     => "*"
      case Div()     => "/"
      case Mod()     => "%"
      case EQ()      => "=="
      case LT()      => "<"
      case GT()      => ">"
      case LE()      => "<="
      case GE()      => ">="
      case NE()      => "!="
      case And()     => "&&"
      case Or()      => "||"
      case Implies() => Errors.fail("Java", "Can't translate ==> expression", x)
    }

  }

  def pp(x: UnaryOp): String = {
    x match {
      case Neg() => "-"
      case Not() => "!"
      case _     => ""
    }
  }

  def pp(x: Type): String = {
    x match {
      case IntType()                               => "int"
      case BoolType()                              => "boolean"
      case VoidType()                              => "void"
      case ClassType(x)                            => pp(x)
      case TidType()                               => "int"
      case MoverType()                             => Errors.fail("Java", "Can't translate Mover type", x)
      case BoogieType(name)                        => Errors.fail("Java", "Can't translate Boogie type", x)
      case x@ArrayType(enclosing, ident, thisName) => s"${pp(x.decl.elemType)}[]"
      case TypeVar(x) => x
      case CollectionType(name, typeArgs) => s"${name}<${typeArgs.map(pp(_))}>"

    }
  }
}

object JavaCompiler {

  def compileToJar(files: List[String],
                   javaArgs : List[String],
                   outputDirectory : String ,
                   javaPackage : String,
                   jarFile : String) = {
    val dir = new File(outputDirectory + "/" + javaPackage)
    val directory =new Directory(dir)
    directory.deleteRecursively()
    dir.mkdirs()
    val anchorJavaFileNames = generateJavaFiles(files.filter(_.endsWith(".anchor")), outputDirectory = outputDirectory, javaPackage = javaPackage)
    val javaFileNames = files.filter(_.endsWith(".java"))
    log(s"Java Source Files: ${javaFileNames.mkString(" ")}")
    val binaryDir = new File("/tmp/bin/")
    (new Directory(binaryDir)).deleteRecursively()
    binaryDir.mkdirs()
    val cmd = s"javac ${javaArgs.mkString(" ")} -d ${binaryDir.getAbsolutePath} ${anchorJavaFileNames.mkString(" ")} ${javaFileNames.mkString(" ")}"
    var result = time(s"${cmd}") {
      val result = cmd !;
      log(s"Result = ${result}")
      result
    }
    if (result == 0) {
      val cmd = s"jar cf ${jarFile} -C ${binaryDir.getAbsolutePath} ."
      result = time(s"${cmd}") {
        val result = cmd !;
        log(s"Result = ${result}")
        result
      }
    }
    result
  }


  private def loadAnchor(lines: String): Program = {
    anchor.lang.FrontEnd.fe(lines)
  }

  private def loadProgram(file: File): Program = {
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()

    val (program, _) = timeAndRecord("Loading") {
      if (file.getPath.endsWith(".anchor")) {
        loadAnchor(lines)
      } else {
        Errors.fail("Input", s"Bad File Name: ${file.getPath}.")
      }
    }
    program
  }

  private def generateJavaFiles(fs: List[String],
                                outputDirectory: String,
                                javaPackage: String) : List[String] = {
    val anchorFragments =
      for (f <- fs) yield time(s"Loading ${f}") {
        loadProgram(new File(f))
      }
    val program = anchorFragments.reduce(_++_)
    val generatedClasses = time(s"Compiling") {
      val compiler = new JavaCompiler(program)
      compiler.gen()
    }
    time("Writing Files") {
      for (GeneratedClass(name, body) <- generatedClasses) yield {
        val fileName = s"${outputDirectory}/${javaPackage}/${name}.java"
        log(s"${name}.java")
        val out = new PrintWriter(new File(fileName))
        out.println(s"package ${javaPackage};")
        out.println("import java.util.concurrent.locks.ReentrantLock;")
        out.println("import java.util.concurrent.atomic.*;")
        out.println()
        out.println(body)
        out.println()
        out.close()
        fileName
      }
    }
  }

}