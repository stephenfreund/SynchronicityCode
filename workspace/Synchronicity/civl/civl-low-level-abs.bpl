procedure {:both} {:layer 11,20} AtomicVC.Leq({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable) returns (res: bool)
{
   assert ValidTid(tid);
   assert shadow.Lock[v1] == tid;
   assert shadow.Lock[v2] == tid;
   assert shadow.Lock[ShadowableTid(tid)] == tid;
   assert is#ShadowableVar(v1) ==> sx.R[x#ShadowableVar(v1)] == SHARED;
   assert !is#ShadowableVar(v2);
   res := (forall j : int :: {f(j)} 0 <= j && f(j) ==> EpochLeq(VCArrayGet(shadow.VC[v1], j), VCArrayGet(shadow.VC[v2], j)));
}

procedure {:yields} {:layer 10} {:refines "AtomicVC.Leq"} VC.Leq({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable) returns (res: bool)
  requires {:layer 10} ValidTid(tid);
  requires {:layer 10} shadow.Lock[v1] == tid;
  requires {:layer 10} shadow.Lock[v2] == tid;
  requires {:layer 10} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
  requires {:layer 10} is#ShadowableVar(v1) ==> sx.R[x#ShadowableVar(v1)] == SHARED;
  requires {:layer 10} !is#ShadowableVar(v2);

  ensures {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10} FTPreserved(tid, old(shadow.Lock), old(shadow.VC), old(sx.W), old(sx.R), shadow.Lock, shadow.VC, sx.W, sx.R);
  ensures {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
{
invariant {:layer 10} 0 <= i;
    invariant {:layer 10} (forall j : int :: {f(j)}
         0 <= j && j < i && f(j) ==>
         EpochLeq(VCArrayGet(shadow.VC[v1], j), VCArrayGet(shadow.VC[v2], j)));
}

procedure {:both} {:layer 11,20} AtomicVC.Copy({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
modifies shadow.VC;
{
    var Vnew: VC;
    var shadow.VC.old : [Shadowable]VC; 

    assert ValidTid(tid);
    assert v1 != v2;
    assert shadow.Lock[ShadowableTid(tid)] == tid;
    assert shadow.Lock[v1] == tid;
    assert shadow.Lock[v2] == tid;
    assert !is#ShadowableVar(v1);	
    assert !is#ShadowableVar(v2);	
    assert VCRepOk(shadow.VC[v2]);
    assert VCRepOk(shadow.VC[v1]);
    if (*) {
        shadow.VC.old := shadow.VC; // save old state now
        shadow.VC[v1] := Vnew;
    	assume VCRepOk(Vnew);
    	assume VCArrayLen(shadow.VC[v1]) == max(VCArrayLen(shadow.VC.old[v1]),VCArrayLen(shadow.VC[v2]));
    	assume (forall j: int :: 0 <= j ==> VCArrayGet(shadow.VC[v1], j) == VCArrayGet(shadow.VC[v2], j));
    } else {
        shadow.VC[v1] := shadow.VC[v2];
    }
}

procedure {:yields} {:layer 10} {:refines "AtomicVC.Copy"} VC.Copy({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
  requires {:layer 10} ValidTid(tid);
  requires {:layer 10} v1 != v2;
  requires {:layer 10} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10} shadow.Lock[v1] == tid;
  requires {:layer 10} shadow.Lock[v2] == tid;
  requires {:layer 10} !is#ShadowableVar(v1);
  requires {:layer 10} !is#ShadowableVar(v2);
  requires {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10} (forall s: Shadowable :: s != v1 && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);
{
    invariant {:layer 10} (forall s: Shadowable :: s != v1 ==> shadow.VC[s] == oldVC[s]);
    invariant {:layer 10} (forall s: Shadowable :: oldLock[s] == tid ==> shadow.Lock[s] == tid);
    invariant {:layer 10} VCRepOk(shadow.VC[v1]);
    invariant {:layer 10} i >= 0;
    invariant {:layer 10} i <= max(len1, len2);
    invariant {:layer 10} VCArrayLen(shadow.VC[v1]) == max(len1, i);
    invariant {:layer 10} (forall j : int :: 0 <= j && j < i      ==> VCArrayGet(shadow.VC[v1], j) == VCArrayGet(shadow.VC[v2], j));
    invariant {:layer 10} (forall j : int :: max(len1, len2) <=j  ==> VCArrayGet(shadow.VC[v1], j) == VCArrayGet(shadow.VC[v2], j));
}


procedure {:right} {:layer 11,20} AtomicVC.Join({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
modifies shadow.VC;
{
    var shadow.VC.old: [Shadowable] VC;
    var vcNew: VC;

    assert ValidTid(tid);
    assert shadow.Lock[ShadowableTid(tid)] == tid;
    assert shadow.Lock[v1] == tid;
    assert shadow.Lock[v2] == tid;
    assert !is#ShadowableVar(v1);	
    assert !is#ShadowableVar(v2);	
    assert VCRepOk(shadow.VC[v2]);
    shadow.VC.old := shadow.VC;
    shadow.VC[v1] := vcNew;    
    assume VCRepOk(shadow.VC[v1]);
    assume VCArrayLen(shadow.VC[v1]) == max(VCArrayLen(shadow.VC.old[v1]),VCArrayLen(shadow.VC.old[v2]));
    assume (forall j: int :: 0 <= j ==> VCArrayGet(shadow.VC[v1], j) == EpochMax(VCArrayGet(shadow.VC.old[v1], j), VCArrayGet(shadow.VC[v2], j)));
}

procedure {:yields} {:layer 10} {:refines "AtomicVC.Join"} VC.Join({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
  requires {:layer 10} ValidTid(tid);
  requires {:layer 10} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10} shadow.Lock[v1] == tid;
  requires {:layer 10} shadow.Lock[v2] == tid;
  requires {:layer 10} !is#ShadowableVar(v1);
  requires {:layer 10} !is#ShadowableVar(v2);
  requires {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10} (forall s: Shadowable :: s != v1 && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);
{
    invariant {:layer 10} (forall s: Shadowable :: s != v1 ==> shadow.VC[s] == oldVC[s]);
    invariant {:layer 10} (forall s: Shadowable :: oldLock[s] == tid ==> shadow.Lock[s] == tid);
    invariant {:layer 10} VCRepOk(shadow.VC[v1]);
    invariant {:layer 10} i >= 0;
    invariant {:layer 10} i <= max(len1, len2);
    invariant {:layer 10} VCArrayLen(shadow.VC[v1]) == max(len1, i);
    invariant {:layer 10} (forall j : int :: 0 <= j && i <= j ==> VCArrayGet(shadow.VC[v1], j) == VCArrayGet(oldVC[v1], j));
    invariant {:layer 10} (forall j : int :: 0 <= j && j < i  ==> VCArrayGet(shadow.VC[v1], j) == EpochMax(VCArrayGet(oldVC[v1], j), VCArrayGet(shadow.VC[v2], j)));
}


procedure {:layer 10} GhostRead() returns (lock : [Shadowable]Tid, data : [Shadowable] [Tid]Epoch)
  ensures lock == shadow.Lock;
  ensures data == shadow.VC;
{
  lock := shadow.Lock;
  data := shadow.VC;
}

procedure {:both} {:layer 11,20} AtomicVC.Inc({:linear "tid" } tid: Tid, v: Shadowable, i: int)
modifies shadow.VC;
{
   assert ValidTid(tid);
   assert shadow.Lock[ShadowableTid(tid)] == tid;
   assert shadow.Lock[v] == tid;
   assert !is#ShadowableVar(v);
   assert i >= 0;
   assert VCRepOk(shadow.VC[v]);
   shadow.VC[v] := VCArraySetLen(shadow.VC[v], max(VCArrayLen(shadow.VC[v]), i+1));
   shadow.VC[v] := shadow.VC[v][i := EpochInc(shadow.VC[v][i])];
}

procedure {:yields} {:layer 10} {:refines "AtomicVC.Inc"} VC.Inc({:linear "tid" } tid: Tid, v: Shadowable, i: int)
  requires {:layer 10} ValidTid(tid);
  requires {:layer 10} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10} shadow.Lock[v] == tid;
  requires {:layer 10} !is#ShadowableVar(v);
  requires {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
  requires {:layer 10} VCRepOk(shadow.VC[v]);
  requires {:layer 10} i >= 0;

  ensures {:layer 10} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10} VCRepOk(shadow.VC[v]);
  ensures {:layer 10} (forall s: Shadowable :: s != v && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);
{
}

procedure {:yields} {:layer 20} {:refines "AtomicFork"} Fork({:linear "tid"} tid:Tid, uid : Tid)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} ValidTid(uid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} shadow.Lock[ShadowableTid(uid)] == tid;
  requires {:layer 10,20} tid != uid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10,20} (forall s: Shadowable :: s != ShadowableTid(tid) && 
  	  	     	     		      s != ShadowableTid(uid) ==>
					      (old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]));

procedure {:yields} {:layer 20} {:refines "AtomicJoin"} Join({:linear "tid"} tid:Tid, uid : Tid)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} ValidTid(uid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} shadow.Lock[ShadowableTid(uid)] == tid;
  requires {:layer 10,20} tid != uid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10,20} (forall s: Shadowable :: s != ShadowableTid(tid) && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);

procedure {:yields} {:layer 20} {:refines "AtomicAcquire"} Acquire({:linear "tid"} tid: Tid, l: Lock)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} shadow.Lock[ShadowableLock(l)] == tid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10,20} (forall s: Shadowable :: s != ShadowableTid(tid) && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);

procedure {:yields} {:layer 20} {:refines "AtomicRelease"} Release({:linear "tid"} tid: Tid, l: Lock)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} shadow.Lock[ShadowableLock(l)] == tid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
  ensures {:layer 10,20} (forall s: Shadowable :: s != ShadowableTid(tid)  && s != ShadowableLock(l) && old(shadow.Lock)[s] == tid ==> old(shadow.VC)[s] == shadow.VC[s]);

procedure {:yields} {:layer 20} {:refines "AtomicWrite"} Write({:linear "tid"} tid:Tid, x : Var) returns (ok : bool)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

procedure {:yields} {:layer 20} {:refines "AtomicRead"} Read({:linear "tid"} tid:Tid, x : Var) returns (ok : bool)
  requires {:layer 10,20} ValidTid(tid);
  requires {:layer 10,20} shadow.Lock[ShadowableTid(tid)] == tid;
  requires {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);

  ensures {:layer 10,20} LocksPreserved(tid, old(shadow.Lock), shadow.Lock);
  ensures {:layer 10,20} FTRepOk(shadow.VC, sx.W, sx.R);
