type Tid = int;  // make int so you can iterate over Tids
const unique nil: Tid;

function {:inline} ValidTid(tid : Tid): bool {
  tid != nil && tid >= 0
}

/* 
 * datatype Epoch = Tid*Clock
 */
type{:datatype} Epoch;
function{:constructor} epoch(tid:Tid, clock:int):Epoch;

const unique SHARED: Epoch;
<
function {:inline} EpochInc(e : Epoch): Epoch {
  epoch(tid#epoch(e),clock#epoch(e) + 1)
}

function {:inline} EpochIsShared(e : Epoch): bool {
  e == SHARED
}

function {:inline} EpochLeq(e1 : Epoch, e2 : Epoch): bool {
  tid#epoch(e1) == tid#epoch(e2) && clock#epoch(e1) <=  clock#epoch(e2)
}

function {:inline} max(a : int, b : int) : int {
  if (a < b) then b else a
}

function {:inline} EpochMax(e1 : Epoch, e2 : Epoch): Epoch {
  epoch(tid#epoch(e1), max(clock#epoch(e1), clock#epoch(e2)))
}

function {:inline} EpochInit(tid: Tid): Epoch {
  epoch(tid, 0)
}


/*
 * VC
 */
type VC = [Tid]Epoch;

// primite accessors to array
// len of VC is stored at -1.
function {:inline} VCArrayLen(vc: VC): int { clock#epoch(vc[-1]) }
function {:inline} VCArraySetLen(vc: VC, n: int): VC { vc[-1 := epoch(-1,n)] }
function {:inline} VCArrayGet(vc: VC, i: int): Epoch { vc[i] }


/*
 * Shadow State
 */
type Lock;
type Var;

/* 
 * datatype Shadowable = tid | lock | var
 */
type{:datatype} Shadowable;
function {:constructor} ShadowableTid(tid: Tid): Shadowable;
function {:constructor} ShadowableLock(l: Lock): Shadowable;
function {:constructor} ShadowableVar(x: Var): Shadowable;

var {:layer 0,20} shadow.VC   : [Shadowable] VC;
var {:layer 0,20} shadow.Lock : [Shadowable] Tid;
var {:layer 0,20} sx.W        : [Var]Epoch;
var {:layer 0,20} sx.R        : [Var]Epoch;

/*
 * Trace Invariant Support
 */
type ThreadStatus = int;  
function{:inline} UNUSED(): ThreadStatus { 0 }
function{:inline} NEW(): ThreadStatus { 1 }
function{:inline} RUNNING(): ThreadStatus { 2 }
function{:inline} STOPPED(): ThreadStatus { 3 }

var {:layer 0,20} thread.State:     [Tid]ThreadStatus;  // status of each thread
var {:layer 0,20} thread.ForkedBy:  [Tid]Tid;           // if forkedBy[t] = u, the u started t
var {:layer 0,20} thread.HasJoined: [Tid,Tid]bool;      // if hasJoined[t,u], then t joined u.


/*
 * Needed for [Read Share]
 */
const unique EMPTY_MAP: [Tid]Epoch;
axiom (forall i : Tid :: EMPTY_MAP[i] == EpochInit(i));

function {:inline} VC.bottom(): VC { VCArraySetLen(EMPTY_MAP,0) }


procedure {:yields} {:layer 10} {:refines "AtomicVC.Leq"} VC.Leq({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable) returns (res: bool)
{
  var vc1, vc2: VC;
  var len1, len2 : int;
  var e1, e2: Epoch;
  var i: int;

  call Yield10(tid);

  call len1 := VCGetSize(tid, v1);
  call len2 := VCGetSize(tid, v1);

  i := 0;
  while (i < max(len1, len2)) 
    invariant {:layer 10} 0 <= i;
    invariant {:layer 10} (forall j : int :: {f(j)}
         0 <= j && j < i && f(j) ==>
         EpochLeq(VCArrayGet(shadow.VC[v1], j), VCArrayGet(shadow.VC[v2], j)));
  {
    call e1 := VCGetElem(tid, v1, i);    
    call e2 := VCGetElem(tid, v2, i);    
    if (!EpochLeq(e1, e2)) {
      assert {:layer 10} f(i);
      res := false;
      call Yield10(tid);
      return;
    }

    i := i + 1;
  }

  res := true;
  call Yield10(tid);
  return;
}

procedure {:yields} {:layer 10} {:refines "AtomicVC.Copy"} VC.Copy({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
{
  var len1, len2 : int;
  var e1, e2: Epoch;
  var i: int;

  var {:layer 10} oldVC : [Shadowable] [Tid]Epoch;
  var {:layer 10} oldLock : [Shadowable] Tid;

  call Yield10(tid);

  call oldLock, oldVC := GhostRead();

  call len1 := VCGetSize(tid, v1);
  call len2 := VCGetSize(tid, v2);

  i := 0;
  while (i < max(len1, len2))
  {
    call e2 := VCGetElem(tid, v2, i);    
    call VCSetElem(tid, v1, i, e2);
    i := i + 1;
  }

  call Yield10(tid);
}


procedure {:yields} {:layer 10} {:refines "AtomicVC.Join"} VC.Join({:linear "tid"} tid: Tid, v1: Shadowable, v2: Shadowable)
{
  var len1, len2 : int;
  var e1, e2: Epoch;
  var i: int;

  var {:layer 10} oldVC : [Shadowable] [Tid]Epoch;
  var {:layer 10} oldLock : [Shadowable] Tid;

  call Yield10(tid);

  call oldLock, oldVC := GhostRead();

  call len1 := VCGetSize(tid, v1);
  call len2 := VCGetSize(tid, v2);

  i := 0;
  while (i < max(len1, len2))
  {
    call e1 := VCGetElem(tid, v1, i);    
    call e2 := VCGetElem(tid, v2, i);    
    call VCSetElem(tid, v1, i, EpochMax(e1, e2));
    i := i + 1;
  }

  call Yield10(tid);
}


procedure {:yields} {:layer 10} {:refines "AtomicVC.Inc"} VC.Inc({:linear "tid" } tid: Tid, v: Shadowable, i: int)
{
  var e: Epoch;

  call Yield10(tid);

  call e := VCGetElem(tid, v, i);

  call VCSetElem(tid, v, i, EpochInc(e));

  call Yield10(tid);
}



procedure {:yields} {:layer 20} {:refines "AtomicFork"} Fork({:linear "tid"} tid:Tid, uid : Tid)
{
  call Yield20(tid);

  call VC.Join(tid, ShadowableTid(uid), ShadowableTid(tid));

  call VC.Inc(tid, ShadowableTid(tid), tid);

  call Yield20(tid);
}


procedure {:yields} {:layer 20} {:refines "AtomicJoin"} Join({:linear "tid"} tid:Tid, uid : Tid)
{
  call Yield20(tid);
  call VC.Join(tid, ShadowableTid(tid), ShadowableTid(uid));
  call Yield20(tid);
}

procedure {:yields} {:layer 20} {:refines "AtomicAcquire"} Acquire({:linear "tid"} tid: Tid, l: Lock)
{
  call Yield20(tid);

  call VC.Join(tid, ShadowableTid(tid), ShadowableLock(l));

  call Yield20(tid);
}

procedure {:yields} {:layer 20} {:refines "AtomicRelease"} Release({:linear "tid"} tid: Tid, l: Lock)
{
  var sm : Shadowable;
  var st : Shadowable;

  call Yield20(tid);

  call VC.Copy(tid, ShadowableLock(l), ShadowableTid(tid));

  call VC.Inc(tid, ShadowableTid(tid), tid);

  call Yield20(tid);
}

procedure {:yields} {:layer 20} {:refines "AtomicWrite"} Write({:linear "tid"} tid:Tid, x : Var) returns (ok : bool)
{
  var e, w, vw, r, vr: Epoch;

  call Yield20(tid);

  call e := ThreadStateGetE(tid);

  // optional block
    call w := VarStateGetWNoLock(tid, x);
    if (w == e) {
      // write same epoch
      ok := true;
      call Yield20(tid);
      return;
    }
   
  call Yield20(tid);

  call AcquireVarLock(tid, x);
  call w := VarStateGetW(tid, x);
  call vw := VCGetElem(tid, ShadowableTid(tid), tid#epoch(w));
  if (!EpochLeq(w, vw)) {
    // write-write race
    call ReleaseVarLock(tid, x); ok := false; call Yield20(tid); return;
  }
  call r := VarStateGetR(tid, x);
  if (r != SHARED) {
    call vr := VCGetElem(tid, ShadowableTid(tid), tid#epoch(r));
    if (!EpochLeq(r, vr)) {
      // read-write race
      call ReleaseVarLock(tid, x); ok := false; call Yield20(tid); return;
    }
    // write exclusive
    call VarStateSetW(tid, x, e);
  } else {
    call ok := VC.Leq(tid, ShadowableVar(x), ShadowableTid(tid));
    if (!ok) {
      // shared-write race
      call ReleaseVarLock(tid, x);
      ok := false;
      call Yield20(tid); return;
    }
    // write shared
    call VarStateSetW(tid, x, e);
  }

  call ReleaseVarLock(tid, x);
  ok := true;
  call Yield20(tid);
  return;
}


procedure {:yields} {:layer 20} {:refines "AtomicRead"} Read({:linear "tid"} tid:Tid, x : Var) returns (ok : bool)
{
  var e, w, vw, r, vr: Epoch;
  var xVC, stVC: VC;

  call Yield20(tid);

  call e := ThreadStateGetE(tid);

   // optional block
     if (*) {
       // read same epoch
       call r := VarStateGetRNoLock(tid, x);
       assume r != SHARED;
       if (r == e) {
         ok := true;
         call Yield20(tid);
         return; 
       }
     } else {
       call r := VarStateGetRShared(tid, x);
       assume r == SHARED;
       call vw := VCGetElemShared(tid, x);
       if (vw == e) {
         ok := true;
         call Yield20(tid);
         return;
       }
     } 

  call Yield20(tid);

  call AcquireVarLock(tid, x);
  call w := VarStateGetW(tid, x);
  call vw := VCGetElem(tid, ShadowableTid(tid), tid#epoch(w));
  if (!EpochLeq(w, vw)) {
    // write-read race
    call ReleaseVarLock(tid, x); 
    ok := false; 
    call Yield20(tid); 
    return;
  }
  call r := VarStateGetR(tid, x);
  if (r != SHARED) {
    call vr := VCGetElem(tid, ShadowableTid(tid), tid#epoch(r));
    if (EpochLeq(r, vr)) {
      // Read Exclusive
      assert {:layer 10,20} tid#epoch(e) >= 0;
      call VarStateSetR(tid, x, e);
      call ReleaseVarLock(tid, x); 
      ok := true; 
      call Yield20(tid); 
      return;
    } else {
      call VCInit(tid, ShadowableVar(x));
      call VCSetElem(tid, ShadowableVar(x), tid#epoch(r), r);
      call VCSetElem(tid, ShadowableVar(x), tid, e); 
      call VarStateSetR(tid, x, SHARED); 
      call ReleaseVarLock(tid, x); 
      ok := true; 
      call Yield20(tid); 
      return;
    }
  } else {
      // Read Shared
      call VCSetElemShared(tid, x, e);
      call ReleaseVarLock(tid, x); 
      ok := true;
      call Yield20(tid);
      return;
  }
}


