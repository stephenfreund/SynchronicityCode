package anchor.boogie

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import anchor.util._
import anchor.sink._

import scala.util.parsing.input.Positional


sealed abstract class BoogieExpr extends Positional
sealed abstract class BoogieConst extends BoogieExpr

case class BoogieNone() extends BoogieExpr
case class BoogieWord(val v: String) extends BoogieConst
case class BoogieInt(val v: Int) extends BoogieConst
case class BoogieBool(val v : Boolean) extends BoogieConst
case class BoogieAsArray(val v : String) extends BoogieConst
case class BoogiePair(val a : BoogieConst, val b : BoogieConst) extends BoogieConst
case class BoogieMap(val v : Map[BoogieConst, BoogieExpr], val elseValue : BoogieExpr) extends BoogieExpr {
  def apply(x : BoogieConst) = v(x)
  def get(x : BoogieConst) = this.v.get(x)
  def getOrElse(x : BoogieConst, v : BoogieExpr) = this.v.getOrElse(x, v)
  def filter(p : ((BoogieConst,BoogieExpr))=>Boolean) = this.v.filter(p)
}
case class BoogieLambda(val x: String, val v : BoogieExpr) extends BoogieExpr
case class BoogieIf(val e : BoogieExpr, val t : BoogieExpr, val f : BoogieExpr) extends BoogieExpr
case class BoogieEq(val e1 : BoogieExpr, val e2 : BoogieExpr) extends BoogieExpr
case class BoogieApp(val terms: List[BoogieExpr]) extends BoogieExpr

case class BoogieSeq(val terms: List[BoogieExpr]) extends BoogieConst

class ModelParser extends JavaTokenParsers {

  val _lambda: Parser[String] = "lambda\\b".r
  val _let: Parser[String] = "let\\b".r
  val _ite: Parser[String] = "ite\\b".r
  val _else: Parser[String] = "else\\b".r
  val _asArray: Parser[String] = "as-array\\b".r
  val _arrow: Parser[String] = "->".r

  val reserved = _lambda | _let | _ite | _asArray | _else | "true" | "false" | _arrow

  val boogieVar: Parser[String] = not(reserved) ~> "[a-zA-Z0-9$!@%+\\._][a-zA-Z0-9$!@%+\\._\\-]*\\b".r

  val boogieWord : Parser[BoogieWord] = boogieVar ^^ { case x => BoogieWord(x) }

  val boogieInt: Parser[BoogieInt] = {
    wholeNumber ^^ { case x => try { BoogieInt(Integer.parseInt(x)) } catch { case _:Throwable => BoogieInt(Integer.MAX_VALUE) } } |
    "(-" ~> wholeNumber <~ ")" ^^ { case x => try { BoogieInt(-Integer.parseInt(x)) } catch { case _:Throwable => BoogieInt(Integer.MIN_VALUE) } }
  }

  val boogieBool: Parser[BoogieBool] = {
    "true" ^^ { case _ => BoogieBool(true) } |
    "false" ^^ { case _ => BoogieBool(false) }
  }

  val boogieAsArray: Parser[BoogieConst] = {
    "(_ (as-array) (" ~ boogieVar ~ "))" ^^ { case _ ~ w ~ _ => BoogieAsArray(w) }
  }

  val boogieConst : Parser[BoogieConst] = {
    boogieWord ||| boogieInt ||| boogieBool ||| boogieAsArray ||| boogieSeq
  }

  val boogieConstOrPair : Parser[BoogieConst] = {
    boogieConst ^^ { x => x } |||
      boogieConst ~ boogieConst ^^ { case a~b => BoogiePair(a,b) }
  }

  val boogieMap : Parser[BoogieMap] = {
    val oneDef: Parser[(BoogieConst, BoogieExpr)] = {
      boogieConstOrPair ~ _arrow ~ boogieExpr ^^ { case w ~ _ ~ v => (w,v) }
    }
    rep(oneDef) ~ opt(_else ~ _arrow ~ boogieExpr ^^ {case _~_~v => v }) ^^ { case x ~ e => BoogieMap(x.toMap, e.getOrElse(BoogieNone())) }
  }

  val boogieLambda : Parser[BoogieLambda] = positioned {
    "(lambda (_ (" ~ boogieVar ~ "(" ~ boogieVar ~ ")))" ~ boogieExpr ~ ")" ^^
      { case _              ~ w    ~  _  ~  _   ~   _   ~ v    ~ _ => BoogieLambda(w, v) }
  }

  val boogieEq : Parser[BoogieEq] = positioned {
    "(=" ~> boogieExpr ~ boogieExpr <~ ")" ^^ { case e1 ~ e2 => BoogieEq(e1, e2) }
  }

  val boogieIf : Parser[BoogieIf] = positioned {
    "(ite" ~> boogieExpr ~ boogieExpr ~ boogieExpr <~ ")" ^^ { case e1 ~ e2 ~ e3 => BoogieIf(e1, e2, e3) }
  }

  val boogieLet : Parser[BoogieExpr] = positioned {
    "(let (_ (" ~ boogieVar  ~ boogieExpr ~ "))" ~ boogieExpr ~ ")" ^^ { case _ ~ x ~ v ~ _ ~ e ~ _ => BoogieModel.subst(x, v, e) }
  }

  val boogieApp : Parser[BoogieApp] = positioned {
    "(" ~> rep1(boogieExpr) <~ ")" ^^ { case xs => BoogieApp(xs) }
  }

  val boogieGarbage : Parser[BoogieNone] = positioned {
    "(_ (as (const) (Array (" ~> boogieVar ~> ") (" ~> boogieVar ~> "))) (" ~> boogieVar ~> "))" ^^ { case _ => BoogieNone() }
  }

  val boogieSeq : Parser[BoogieSeq] = positioned {
    "(as (seq.empty) (Seq (" ~> boogieVar <~ ")))" ^^ { case _ => BoogieSeq(Nil) } |||
      "(seq.unit " ~> boogieConst <~ ")" ^^ { case x => BoogieSeq(List(x)) } |||
      "(seq.++ " ~> boogieSeq ~ boogieSeq <~ ")" ^^ { case x ~ y => BoogieSeq(x.terms ++ y.terms) } |||
      "(" ~> boogieWord <~ ")" ^^ { case x => BoogieSeq(List(x)) } |||
      boogieLet ^^ { case x => BoogieSeq(Nil) }
  }

  val boogieWackyArray: Parser[BoogieMap] = {
    "(_ (as (const) (Array" ~ boogieExpr ~ boogieExpr ~> ")) (" ~> boogieConst <~ "))" ^^ { case w => BoogieMap(Map(), w) }
  }

  val boogieExpr : Parser[BoogieExpr] = positioned {
    boogieConst ||| (boogieWackyArray | (("{" ~> boogieMap <~ "}") ||| boogieLambda ||| boogieEq ||| boogieIf ||| boogieLet ||| boogieApp  ||| boogieGarbage))
  }

  val model : Parser[BoogieMap] = {
    "*** MODEL" ~> boogieMap <~ "*** END_MODEL"
  }


  def parse(prog: String): BoogieMap = {
    parse(model, prog) match {
      case Success(matched, _) => matched
      case Failure(msg, loc)     => Errors.error("Syntax", msg + s"\n${prog}", NoNode(loc.pos)); null
      case Error(msg, loc)       => Errors.error("Syntax", msg + s"\n${prog}", NoNode(loc.pos)); null
    }
  }
}

object BoogieModel {
  def eval(context : BoogieMap, e : BoogieExpr) : BoogieExpr = e match {
    case x@BoogieNone() => x
    case x@BoogieWord(v)      => context.v.getOrElse(x, x)
    case x@BoogieInt(v)       => x
    case x@BoogieBool(v)      => x
    case x@BoogieAsArray(v)   => context.v(BoogieWord(v))
    case x@BoogiePair(_,_)    => x
    case x@BoogieMap(_, _)    => x
    case x@BoogieLambda(_, _) => x
    case x@BoogieSeq(_) => x
    case x@BoogieIf(e, t, f)  => if (eval(context, e) == BoogieBool(true)) {
      eval(context, t)
    } else {
      eval(context, f)
    }
    case x@BoogieEq(e1, e2)   => BoogieBool(eval(context, e1) == eval(context, e2))
    case x@BoogieApp(terms)   => {
      val evaluated = terms.map(eval(context, _))
      evaluated match {
        case result :: Nil => result
        case (map: BoogieMap) :: (key : BoogieWord) :: Nil => map.v.getOrElse(key, map.elseValue)
        case (map: BoogieMap) :: (key : BoogieInt) :: Nil => map.v.getOrElse(key, map.elseValue)
        case (fun: BoogieLambda) :: (e : BoogieExpr) :: Nil => eval(context, subst(fun.x, e, fun.v))
        case _ => Errors.warn("boogie", s"Cannot parse Boogie counter-example term: ${e}"); BoogieNone()
      }
    }
  }

  def subst(x: String, e: BoogieExpr, body: BoogieExpr) : BoogieExpr = body match {
    case t@BoogieNone()            => t
    case BoogieWord(v) if v == x   => e
    case t@BoogieWord(v)           => t
    case t@BoogieInt(v)            => t
    case t@BoogieBool(v)           => t
    case t@BoogieAsArray(v)        => t
    case t@BoogieMap(v, elseValue) => t
    case t@BoogieSeq(v)            => BoogieSeq(v.map(subst(x, e, _)))
    case t@BoogieLambda(_, _)      => BoogieLambda(t.x, subst(x, e, t.v))
    case BoogieIf(b, t, f)         => BoogieIf(subst(x, e, b), subst(x, e, t), subst(x, e, f))
    case BoogieEq(e1, e2)          => BoogieEq(subst(x, e, e1), subst(x, e, e2))
    case BoogieApp(terms)          => BoogieApp(terms.map(subst(x, e, _)))
  }

  def parse(prog: String): BoogieMap = {
    new ModelParser().parse(prog)
  }
}
