package anchor.sink

import anchor.util.Errors._

class SymTab private (val parent : Option[SymTab],
                      val classes : Map[String, ClassDecl],
                      val arrays : Map[String, ArrayDecl],
                      val fields : Map[String, FieldDecl],
                      val methods : Map[String, MethodDecl],
                      val locals : Map[String, VarDecl],
                      val collections : Map[String, Collection],
                      val builtins: Map[String, BuiltInFunctionDecl]) {

  def this() {
    this(None, Map(), Map(), Map(), Map(), Map(), Map(), Map())
  }

  private def toString(depth : Int) : String = {
    def pp(a : Map[String,_], s: String) = {
      a.keys.map(_ + ": " + s)
    }
    val pad = ("  " * depth)
    val decls = pp(classes,"class") ++ pp(arrays, "array") ++ pp(fields, "field") ++ pp(methods, "method") ++ pp(locals, "local")
    val result = decls.mkString(", ")
    pad + result + (if (parent != None) s"\n${parent.get.toString(depth+1)}" else "")
  }

  override def toString() : String = {
    toString(0);
  }

  def this(classes : List[ClassDecl], globals: List[VarDecl], library : Library) {
    this(None, classes.map(x => { x.name -> x }).toMap, Map(), Map(), Map(), globals.map(x => { x.name -> x }).toMap, library.collections, library.functions.map(x => { x.name -> x}).toMap)
    for (c <- classes) {
      check("Scoping", classes.count(_.name == c.name) == 1, s"Duplicate class ${c.name}", c)
    }
    for (v <- globals) {
      check("Scoping", globals.count(_.name == v.name) == 1, s"Duplicate globals ${v.name}", v)
    }
  }

  def pushClasses(classes : List[ClassDecl]) : SymTab = {
    for (c <- classes) {
      check("Scoping", resolveClass(c.name) == None && classes.count(_.name == c.name) == 1, s"Duplicate class ${c.name}", c)
    }
    new SymTab(Some(this), classes.map(x => { x.name -> x }).toMap, Map(), Map(), Map(), Map(), Map(), Map())
  }

  def pushArrays(arrays : List[ArrayDecl]) : SymTab = {
    for (a <- arrays) {
      check("Scoping", resolveArray(a.name) == None && arrays.count(_.name == a.name) == 1, s"Duplicate array ${a.name}", a)
    }
    new SymTab(Some(this),  Map(), arrays.map(x => { x.name -> x }).toMap, Map(), Map(), Map(), Map(), Map())
  }

  def pushFields(fields : List[FieldDecl]) : SymTab = {
    for (f <- fields) {
      check("Scoping", resolveField(f.name) == None && fields.count(_.name == f.name) == 1, s"Duplicate field ${f.name}", f)
    }
    new SymTab(Some(this), Map(), Map(), fields.map(x => { x.name -> x }).toMap, Map(), Map(), Map(), Map())
  }

  def pushMethods(methods : List[MethodDecl]) : SymTab = {
    for (m <- methods) {
      check("Scoping", resolveMethod(m.name) == None &&  methods.count(_.name == m.name) == 1, s"Duplicate method ${m.name}", m)
    }
    new SymTab(Some(this), Map(), Map(), Map(), methods.map(x => { x.name -> x }).toMap, Map(), Map(), Map())
  }

  def pushLocals(locals : List[VarDecl]) : SymTab = {
    for (v <- locals) {
      check("Scoping", resolveVar(v.name) == None, s"Duplicate var ${v.name} in ${assert(false); this}", v)
    }
    new SymTab(Some(this), Map(), Map(), Map(), Map(), locals.map(x => { x.name -> x }).toMap, Map(), Map())
  }

  def resolveClass(x : String) : Option[ClassDecl] = {
    if (classes.contains(x)) {
      Some(classes(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveClass(x)
      }
    }
  }

  def resolveArray(x : String) : Option[ArrayDecl] = {
    if (arrays.contains(x)) {
      Some(arrays(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveArray(x)
      }
    }
  }

  def resolveField(x : String) : Option[FieldDecl] = {
    if (fields.contains(x)) {
      Some(fields(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveField(x)
      }
    }
  }

  def resolveMethod(x : String) : Option[MethodDecl] = {
    if (methods.contains(x)) {
      Some(methods(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveMethod(x)
      }
    }
  }

  def resolveVar(x : String) : Option[VarDecl] = {
    if (locals.contains(x)) {
      Some(locals(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveVar(x)
      }
    }
  }

  def resolveBuiltin(x : String) : Option[BuiltInFunctionDecl] = {
    if (builtins.contains(x)) {
      Some(builtins(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveBuiltin(x)
      }
    }
  }

  def resolveCollection(x : String) : Option[Collection] = {
    if (collections.contains(x)) {
      Some(collections(x))
    } else {
      parent match {
        case None => None
        case Some(p) => p.resolveCollection(x)
      }
    }
  }


  def isGlobal(x : VarAccess) : Boolean = {
    x.decl.scope.parent == None
  }

  def renameVar(from : String, to : String) : SymTab = {
    locals.find(_._1 == from) match {
      case None => {
        parent match {
          case Some(value) => new SymTab(Some(value.renameVar(from, to)), this.classes, this.arrays, this.fields, this.methods, this.locals, this.collections, this.builtins)
          case None        => this
        }
      }
      case Some((_,decl)) => {
        new SymTab(this.parent, this.classes, this.arrays, this.fields, this.methods, this.locals.filter(_._1 != from) + { to -> AST.pos(VarDecl(decl.t, to), decl.pos) }, this.collections, this.builtins)
      }
    }
  }

  def allLocals : List[VarDecl] = locals.values.toList ++ parent.map(_.allLocals).getOrElse(Nil)
}

