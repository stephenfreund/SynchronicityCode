package anchor.lang

abstract class Collection(val name : String, val numParams : Int) {
  def functions: List[BuiltInFunctionDecl]
  def boogieText : String
}

object SetCollection extends Collection("Set", 1) {

  val anchorText =
    """
      |SetEmpty<#T>() : Set<#T>;
      |SetSingleton<#T>(#T) : Set<#T>;
      |SetUnion<#T>(Set<#T>, Set<#T>) : Set<#T>;
      |SetAdd<#T>(Set<#T>, #T) : Set<#T>;
      |SetRemove<#T>(Set<#T>, #T) : Set<#T>;
      |SetContains<#T>(Set<#T>, #T) : bool;
      |SetIsEqual<#T>(Set<#T>, Set<#T>) : bool;
      |SetIsSubset<#T>(Set<#T>, Set<#T>) : bool;
    """.stripMargin

  val boogieText =
    """
      |type Set.#0 = [#0]bool;
      |
      |function {:builtin "MapConst"} mapconstbool.Set.#0(bool) : Set.#0;
      |function {:builtin "MapAnd"}   mapand.Set.#0(Set.#0, Set.#0) : Set.#0;
      |function {:builtin "MapOr"}    mapor.Set.#0(Set.#0, Set.#0) : Set.#0;
      |function {:builtin "MapNot"}   mapnot.Set.#0(Set.#0) : Set.#0;
      |function {:builtin "MapIte"}   mapitebool.Set.#0(Set.#0, Set.#0, Set.#0) : Set.#0;
      |function {:builtin "MapIff"}   mapiff.Set.#0(Set.#0, Set.#0) : Set.#0;
      |function {:builtin "MapImp"}   mapimp.Set.#0(Set.#0, Set.#0) : Set.#0;
      |
      |const SetEmptyConst.#0: Set.#0;
      |axiom SetEmptyConst.#0 == mapconstbool.Set.#0(false);
      |
      |function {:inline} SetEmpty.#0() : Set.#0 {
      |  SetEmptyConst.#0
      |}
      |
      |function {:inline} SetSingleton.#0(x: #0) : Set.#0 {
      |  SetEmpty.#0()[x := true]
      |}
      |
      |function {:inline} SetUnion.#0(a: Set.#0, b: Set.#0) : Set.#0 {
      |  mapor.Set.#0(a, b)
      |}
      |
      |function {:inline} SetAdd.#0(a: Set.#0, x: #0) : Set.#0 {
      |  SetUnion.#0(a, SetSingleton.#0(x))
      |}
      |
      |function {:inline} SetRemove.#0(a: Set.#0, x: #0) : Set.#0 {
      |  mapand.Set.#0(a, mapnot.Set.#0(SetEmpty.#0())[x := false])
      |}
      |
      |function {:inline} SetContains.#0(a: Set.#0, x: #0) : bool {
      |  a[x]
      |}
      |
      |function {:inline} SetSubset.#0(a: Set.#0, b: Set.#0) : bool {
      |  (forall x : #0 :: mapimp.Set.#0(a,b)[x])
      |}
      |
      |function {:inline} SetEqual.#0(a: Set.#0, b: Set.#0) : bool {
      |  (forall x : #0 :: mapiff.Set.#0(a,b)[x])
      |}
      |
    """.stripMargin

  val functions = Parser.lib(anchorText)

}

///

object SeqCollection extends Collection("Seq", 1) {

  val anchorText =
    """
      |SeqEmpty<#T>() : Seq<#T>;
      |SeqLen<#T>(Seq<#T>): int;
      |SeqConcat<#T>(Seq<#T>, Seq<#T>) : Seq<#T>;
      |SeqNth<#T>(Seq<#T>, int) : #T;
      |SeqUnit<#T>(#T) : Seq<#T>;
      |SeqExtract<#T>(Seq<#T>, int, int) : Seq<#T>;
      |SeqSub<#T>(Seq<#T>, int, int) : Seq<#T>;
      |SeqEqual<#T>(Seq<#T>,Seq<#T>): bool;
    """.stripMargin

  val boogieText =
    """
      |type {:builtin "(Seq #z0)"} Seq.#0;
      |
      |function {:builtin "(as seq.empty (Seq #z0))"} SeqEmpty.#0(): Seq.#0;
      |function {:builtin "seq.len"} SeqLen.#0(s: Seq.#0): int;
      |function {:builtin "seq.++"} SeqConcat.#0(s1: Seq.#0, s2:Seq.#0): Seq.#0;
      |function {:builtin "seq.unit"} SeqUnit.#0(v: #0): Seq.#0;
      |function {:builtin "seq.nth"} SeqNth.#0(s: Seq.#0, i: int): #0;
      |function {:builtin "seq.extract"} SeqExtract.#0(s: Seq.#0, pos: int, len: int): Seq.#0;
      |
      |function {:inline} SeqEqual.#0(a: Seq.#0, b: Seq.#0) : bool {
      |  SeqLen.#0(a) == SeqLen.#0(b) &&
      |  (forall i : int :: 0 <= i && i < SeqLen.#0(a) ==> (SeqNth.#0(a, i) == SeqNth.#0(b, i)))
      |}
      |
      |function {:inline} SeqSub.#0(a: Seq.#0, start: int, end: int) : Seq.#0 {
      |  SeqExtract.#0(a, start, end-start)
      |}
      |
      |
    """.stripMargin

  val functions = Parser.lib(anchorText)

}

object MapCollection extends Collection("Map", 2) {

  val anchorText =
    """
      |MapEmpty<#K,#V>() : Map<#K,#V>;
      |MapStore<#K,#V>(Map<#K,#V>, #K, #V) : Map<#K,#V>;
      |MapSelect<#K,#V>(Map<#K,#V>, #K) : #V;
    """.stripMargin

  val boogieText =
    """
      |type Map.#0.#1 = [#0]#1;
      |
      |
      |function {:builtin "MapConst"} mapconst.#0.#1(#1): Map.#0.#1;
      |
      |function {:inline} MapEmpty.#0.#1() : Map.#0.#1 {
      |  mapconst.#0.#1(#d1)
      |}
      |
      |function {:inline} MapStore.#0.#1(m : Map.#0.#1, k : #0, v : #1) : Map.#0.#1 {
      |  m[k := v]
      |}
      |
      |function {:inline} MapSelect.#0.#1(m : Map.#0.#1, k : #0) : #1 {
      |  m[k]
      |}
      |
    """.stripMargin

  val functions = Parser.lib(anchorText)

}


object PairCollection extends Collection("Pair", 2) {

  val anchorText =
    """
      |PairCons<#A,#B>(#A, #B) : Pair<#A,#B>;
      |PairFirst<#A,#B>(Pair<#A,#B>) : #A;
      |PairSecond<#A,#B>(Pair<#A,#B>) : #B;
    """.stripMargin

  val boogieText =
    """
      |type{:datatype} Pair.#0.#1;
      |function{:constructor} Pair.#0.#1.cons(first:#0, second:#1) : Pair.#0.#1;
      |
      |function {:inline} PairEmpty.#0.#1() : Pair.#0.#1 {
      |  PairCons.#0.#1(#d0, #d1)
      |}
      |
      |function {:inline} PairCons.#0.#1(a : #0, b : #1): Pair.#0.#1 {
      |  Pair.#0.#1.cons(a,b)
      |}
      |
      |function {:inline} PairFirst.#0.#1(m : Pair.#0.#1): #0 {
      |  first#Pair.#0.#1.cons(m)
      |}
      |
      |function {:inline} PairSecond.#0.#1(m : Pair.#0.#1): #1 {
      |  second#Pair.#0.#1.cons(m)
      |}
      |
    """.stripMargin

  val functions = Parser.lib(anchorText)

}



object Library {
  def collections = Map("Set" -> SetCollection, "Seq" -> SeqCollection, "Map" -> MapCollection /* "Pair" -> PairCollection */)

  def functions = collections.flatMap(_._2.functions)

  {

  }
}
