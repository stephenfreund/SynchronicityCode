/*
 * State Invariants
 */
function {:inline false} VarsRepOk(w: [Var]Epoch, r: [Var]Epoch ): bool {
  (forall v: Var :: ValidTid(tid#epoch(w[v]))) &&
  (forall v: Var :: r[v] == SHARED || (tid#epoch(r[v]) >= 0 && tid#epoch(r[v]) != nil))
}

function {:inline false} VCRepOk(vc: VC): bool { 
  VCArrayLen(vc) >= 0 &&
  (forall j: int :: {vc[j]} 0 <= j && j < VCArrayLen(vc) ==> clock#epoch(vc[j]) >= 0) &&
  (forall j: int :: {vc[j]} 0 <= j && j < VCArrayLen(vc) ==> tid#epoch(vc[j]) == j) &&
  (forall j: int :: VCArrayLen(vc) <= j ==> vc[j] == EpochInit(j))
}

function {:inline} VCsRepOk(vcs: [Shadowable]VC): bool {
  (forall s : Shadowable :: VCRepOk(vcs[s]))
}

function {:inline} FTRepOk(vcs: [Shadowable]VC, w: [Var]Epoch, r: [Var]Epoch): bool {
  VCsRepOk(vcs) &&
  VarsRepOk(w, r)
}


/*
 * Environment Assumptions -- what's preserved across yields
 */
function {:inline} LocksPreserved({:linear "tid" } tid: Tid, oldLocks: [Shadowable] Tid, locks: [Shadowable]Tid): bool {
  (forall v: Shadowable :: oldLocks[v] == tid ==> locks[v] == tid)
}

function {:inline} SharedInvPreserved(oldR: [Var] Epoch, r: [Var] Epoch): bool {
  (forall x: Var :: oldR[x] == SHARED ==> r[x] == SHARED) 
}

function {:inline} FTPreserved({:linear "tid" } tid:Tid,
	 	  	              oldLocks: [Shadowable] Tid, oldVcs: [Shadowable]VC, oldW: [Var]Epoch, oldR: [Var]Epoch,
	 	  	              locks: [Shadowable] Tid, 	  vcs: [Shadowable]VC, 	  w: [Var]Epoch,    r: [Var]Epoch): bool {
  LocksPreserved(tid, oldLocks, locks) && 
  SharedInvPreserved(oldR, r) &&
  (forall s: Shadowable :: oldLocks[s] == tid ==> vcs[s] == oldVcs[s]) && 
  (forall x: Var :: oldLocks[ShadowableVar(x)] == tid ==> r[x] == oldR[x]) &&
  (forall x: Var :: oldLocks[ShadowableVar(x)] == tid ==> w[x] == oldW[x])
}

 procedure {:yields} {:layer 10} Yield10({:linear "tid"} tid:Tid)
     requires {:layer 10} ValidTid(tid);
     requires {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
     ensures {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
     ensures {:layer 10} FTPreserved(tid, old(shadow.Lock), old(shadow.VC), old(sx.W), old(sx.R), shadow.Lock, shadow.VC, sx.W, sx.R);
     ensures {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
 {
   yield;
   assert {:layer 10} ValidTid(tid);
   assert {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
   assert {:layer 10} FTPreserved(tid, old(shadow.Lock), old(shadow.VC), old(sx.W), old(sx.R), shadow.Lock, shadow.VC, sx.W, sx.R);
   assert {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
 }


procedure {:yields} {:layer 20} Yield20({:linear "tid"} tid:Tid)
    requires {:layer 10,20} ValidTid(tid);
    requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
    requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

    ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
    ensures {:layer 10,20} FTPreserved(tid, old(shadow.Lock), old(shadow.VC), old(sx.W), old(sx.R), shadow.Lock, shadow.VC, sx.W, sx.R);
    ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
{
    yield;
    assert {:layer 10,20} ValidTid(tid);
    assert {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
    assert {:layer 10,20} FTPreserved(tid, old(shadow.Lock), old(shadow.VC), old(sx.W), old(sx.R), shadow.Lock, shadow.VC, sx.W, sx.R);
    assert {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
}
