package anchor.sink

abstract class Symbol
case class Epsilon() extends Symbol
case class Step(val t: Int) extends Symbol

case class State(val n : Int)

class SpecNFA {

  private var transitions: Map[State,Map[Symbol,List[State]]] = Map.empty
  private var symbols : Set[Symbol] = Set()
  private var statesInAllocOrder = List[State]()
  private val start = addState()
  private var acceptStates : Set[State] = Set.empty

  def build(in: State, index: Int, t : Transaction) : State = {
    val end = addState()
    if (!t.repeats) {
      addTransition(in, end, Step(index))
      if (t.modifies.contains(VarAccess("$result"))) {
        acceptStates = acceptStates + end
      }
    } else {
      val t1 = addState()
      val t2 = addState()
      addTransition(in, t1, Epsilon())
      addTransition(t1, t2, Step(index))
      addTransition(t2, end, Epsilon())
      addTransition(in, end, Epsilon())
      addTransition(t2, t1, Epsilon())
    }
    end
  }

  def this(spec: ExplicitMethodSpec)
  {
    this()
    var current = start
    for ((t,i) <- spec.transactions.zipWithIndex) {
      current = build(current, i, t)
    }
    acceptStates = acceptStates + current
  }

  def addState() : State = {
    val n = State(transitions.size)
    transitions = transitions + { n -> Map.empty }
    statesInAllocOrder = statesInAllocOrder :+ n
    n
  }

  def addTransition(src: State, dst: State, label: Symbol) = {
    assert(transitions.contains(src))
    assert(transitions.contains(dst))
    symbols = symbols + label
    val current = transitions.getOrElse(src, Map.empty).getOrElse(label, List.empty)
    transitions = transitions + { src -> (transitions(src) + { (label, dst::current) }) }
  }

  def states() = {
    transitions.keys
  }

  def acceptingStates() = {
    acceptStates
  }

  def outEdges(src: State) : List[(Symbol,List[State])] = {
    transitions(src).toList
  }

  override def toString: String = {
    (for (s <- statesInAllocOrder) yield {
      s"${s.n}: ${if (this.acceptStates.contains(s)) "ACCEPT" else ""}\n" +
        (for ((l,d) <- transitions(s)) yield s"    ${l} -> ${d.map(s => s"${s.n}").mkString(", ")}\n").mkString("")
    }).mkString("")
  }

  def size = states.size

  def epsilonClosure(in : Set[State]): Set[State] = {
    var out = in
    for (s <- in) {
      out = out ++ transitions(s).getOrElse(Epsilon(),Set.empty)
    }
    if (in != out) {
      epsilonClosure(out)
    } else {
      out
    }
  }

  def T(src: State, label:Symbol) : Set[State] = {
    epsilonClosure(transitions(src)(label).toSet)
  }

  def T(src: State) : List[(Symbol,State)] = {
    (for (label <- transitions(src).keys;
         if label != Epsilon()) yield {
      val dsts = epsilonClosure(transitions(src)(label).toSet)
      dsts.map(d => (label,d))
    }).flatten[(Symbol,State)].toList
  }

  def T(src: Set[State], label:Symbol) : Set[State] = {
    epsilonClosure(src.flatMap(transitions(_)(label)).toSet)
  }

  def into(dst: State) = {
    for (src <- transitions.keys;
         (label, dsts) <- transitions(src);
         if label != Epsilon();
         if T(src, label).contains(dst)) yield {
      (src, label)
    }
  }

  def dfa() : SpecNFA = {
    val nfa = new SpecNFA()
    var stateMap = Map[Set[State],State]()

    stateMap += { epsilonClosure(Set(start)) -> nfa.start }

    def stateFor(old: Set[State]) : State = {
      stateMap.get(old) match {
        case Some(value) => value
        case None        => {
          val newState = nfa.addState()
          stateMap += { old -> newState }
          if (!old.intersect(acceptStates).isEmpty) {
            nfa.acceptStates = nfa.acceptStates + newState
          }
          newState
        }
      }
    }

    def build(dfaStates: Set[State]) : Unit = {
      val src = stateFor(dfaStates)
      for (symbol <- symbols if symbol != Epsilon()) {
        val nextStates = dfaStates.map(transitions(_).getOrElse(symbol, List.empty)).flatten
        val closedNextStates = epsilonClosure(nextStates)
        val newState = !stateMap.contains(closedNextStates)
        if (!closedNextStates.isEmpty) {
          val dst = stateFor(closedNextStates)
          nfa.addTransition(src, dst, symbol)
          if (newState) {
            build(closedNextStates)
          }
        }
      }
    }

    build(epsilonClosure(Set(start)))

    nfa
  }

  def epsilonFree() : SpecNFA = {
    val nfa = new SpecNFA()
    nfa.statesInAllocOrder = this.statesInAllocOrder
    nfa.acceptStates = this.acceptStates
    nfa.transitions = nfa.statesInAllocOrder.map(s => { s -> Map[Symbol,List[State]]() }).toMap
    nfa.symbols = this.symbols

    def build(src: State) : Unit = {
      val closed = epsilonClosure(Set(src))
      for (s <- closed; symbol <- symbols if symbol != Epsilon()) {
        val nextStates = transitions(s).getOrElse(symbol, List.empty)
        val closedNextStates = epsilonClosure(nextStates.toSet)
        for (dst <- closedNextStates) {
          nfa.addTransition(src, dst, symbol)
        }
      }
    }

    for (s <- states()) {
      build(s)
    }

    nfa
  }


  def toDot() : String = {
    s"""
       |digraph G {
       |  ${(for (s <- states()) yield
                s"""s${s.n}[${if (acceptStates.contains(s)) "shape=doublecircle" else ""}];""").mkString("\n")}
       |
       |  ${(for (s <- states();
                (label,dsts) <- transitions(s);
                d <- dsts) yield s"""s${s.n} -> s${d.n} [label=\"${label}\"];""").mkString("\n")}
       | }
     """.stripMargin
  }
}
