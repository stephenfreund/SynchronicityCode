package acme.scala

import acme.util._
import acme.util.time.TimedExpr
import acme.util.Util._

object Util {

  def justify(s: String, width: Int): List[String] = {
    if (s.length < width) List(s)
    else {
      val space = s.substring(0, width).lastIndexOf(' ');
      val cut = if (space == -1) width else space;
      (s.substring(0, cut) + "\n") :: justify(s.substring(cut + 1), width);
    }
  }

  def logf(format: String, os: AnyRef*): Unit = {
    acme.util.Util.logf(format, os.toSeq: _*);
  }

  def log(o: Any): Unit = Util.logf("%s", o.toString());

  def warnf(format: String, os: AnyRef*): Unit = {
    Assert.warn(format, os.toSeq: _*);
  }

  def warn(o: Any): Unit = Assert.warn("%s", o.toString());

  def debug(key: String, s: => String) = {
    if (Debug.debugOn(key)) {
      Debug.debug(key, s);
    }
  }

  def debugIf(key: String)(s: => Unit): Unit = {
    if (Debug.debugOn(key)) {
      s;
    }
  }

  def debugf(key: String, format: String, os: AnyRef*) = {
    if (Debug.debugOn(key)) {
      Debug.debugf(key, format, os.toSeq: _*);
    }
  }

  def time[T](name: String)(t: => T): T = {
    new TimedExpr[T](name) {
      def run() = {
        t;
      }
    }.eval();
  }

  def timeAndRecord[T](name: String)(t: => T): (T, Long) = {
    val tt = new Timer("Timer", name);
    tt.time(new TimedExpr[T](name) {
      def run() = {
        t;
      }
    }.eval());
  }

  def timer[T](name: String): acme.util.count.Timer = {
    new acme.util.count.Timer("Timer", name);
  }

  def padRight(s1: AnyRef, s2: AnyRef): (String, String) = {
    padRight(s1.toString(), s2.toString());
  }

  def padRight(s1: String, s2: String): (String, String) = {
    val n = Math.max(s1.length, s2.length);
    (s1.padTo(n, ' '), s2.padTo(n, ' '));
  }

  def padLeft(s1: String, s2: String): (String, String) = {
    val n = Math.max(s1.length, s2.length);
    (s1.reverse.padTo(n, ' ').reverse, s2.reverse.padTo(n, ' ').reverse);
  }

  def padColumn(col: List[String]) = {
    val max = if (col == Nil) 0 else col.max(Ordering.by((_: String).length)).length();
    col.map(_.padTo(max, ' '));
  }

  def padTable(table: List[List[String]], seps: List[String]): List[String] = {
    seps match {
      case Nil => Nil.padTo(table.length, "");
      case x :: xs => {
        val firstCol = padColumn(table.map(_(0)));
        val restCols = padTable(table.map(_.tail), xs);
        firstCol.zip(restCols).map(e => e._1 + x + e._2);
      }
    }
  }

  def escapeFileName(s: String) = {
    s.replace("<", "_").replace(">", "_").replace("/", "_");
  }

  def quiet[T](b: Boolean)(t: => T): T = {
    val old = quietOption.get();
    quietOption.set(b);
    val x = t;
    quietOption.set(old);
    x;
  }

  private var counts = scala.collection.mutable.Map[String, Int]()

  def fresh(s : String) : String = {
    val count = counts.getOrElse(s, 0)
    counts(s) = count + 1
    s"${s}_${count}"
  }

  private var intCount = 0

  def fresh() : Int = {
    val r = intCount
    intCount += 1
    r
  }


  def main(args: Array[String]) = {
    val t = List(
      List("A", "B", "CCCCC"),
      List("AAAAAAAAAAAAAAAA", "B", "CCCCC"),
      List("AAAAAAAAAAAAAAAA", "VVVB", "CCCCC"));

    println(padTable(t, List("*", ":::", " -> ", "\n")).mkString);
  }

}



class Timer(val group : String, val name : String) {
	val timer = new acme.util.count.Timer(group, name);
	
	def time[T](t : => T) : (T,Long) = {
	  val y = timer.start();
	  val x = { t };
	  (x, timer.stop(y));
	}
} 
