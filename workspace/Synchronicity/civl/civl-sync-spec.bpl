/****** Layer 0  ******/

// VarState Lock
procedure {:yields}  {:layer 0} {:refines "AtomicAcquireVarLock"} AcquireVarLock({:linear "tid"} tid: Tid, x : Var);
procedure {:right} {:layer 1,20} AtomicAcquireVarLock({:linear "tid"} tid: Tid, x : Var)
modifies shadow.Lock;
{ assert ValidTid(tid); assume shadow.Lock[ShadowableVar(x)] == nil; shadow.Lock[ShadowableVar(x)] := tid; }

procedure {:yields} {:layer 0} {:refines "AtomicReleaseVarLock"} ReleaseVarLock({:linear "tid"} tid: Tid, x : Var);
procedure {:left} {:layer 1,20} AtomicReleaseVarLock({:linear "tid"} tid: Tid, x : Var)
modifies shadow.Lock;
{ assert ValidTid(tid); assert shadow.Lock[ShadowableVar(x)] == tid; shadow.Lock[ShadowableVar(x)] := nil; }


// ThreadState
procedure {:yields} {:layer 0} {:refines "AtomicThreadStateGetE"} ThreadStateGetE({:linear "tid"} tid: Tid) returns (e:Epoch);
procedure {:both} {:layer 1,20} AtomicThreadStateGetE({:linear "tid"} tid: Tid) returns (e:Epoch)
{ assert ValidTid(tid); assert shadow.Lock[ShadowableTid(tid)] == tid; e := shadow.VC[ShadowableTid(tid)][tid]; }


// VarState
procedure {:yields} {:layer 0} {:refines "AtomicVarStateSetW"} VarStateSetW({:linear "tid"} tid:Tid, x : Var, e:Epoch);
procedure {:atomic} {:layer 1,20} AtomicVarStateSetW({:linear "tid"} tid:Tid, x : Var, e:Epoch)
modifies sx.W;
{ assert ValidTid(tid); assert shadow.Lock[ShadowableVar(x)] == tid; sx.W[x] := e; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateGetW"} VarStateGetW({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch);
procedure {:both} {:layer 1,20} AtomicVarStateGetW({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch)
{ assert ValidTid(tid); assert shadow.Lock[ShadowableVar(x)] == tid; e := sx.W[x]; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateGetWNoLock"} VarStateGetWNoLock({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch);
procedure {:atomic} {:layer 1,20} AtomicVarStateGetWNoLock({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch)
{ assert ValidTid(tid); e := sx.W[x]; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateSetR"} VarStateSetR({:linear "tid"} tid:Tid, x : Var, e:Epoch);
procedure {:atomic} {:layer 1,20} AtomicVarStateSetR({:linear "tid"} tid:Tid, x : Var, e:Epoch)
modifies sx.R;
{ assert ValidTid(tid); assert shadow.Lock[ShadowableVar(x)] == tid; assert sx.R[x] != SHARED; sx.R[x] := e; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateGetRNoLock"} VarStateGetRNoLock({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch);
procedure {:atomic} {:layer 1,20} AtomicVarStateGetRNoLock({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch)
{ assert ValidTid(tid); e := sx.R[x]; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateGetR"} VarStateGetR({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch);
procedure {:both} {:layer 1,20} AtomicVarStateGetR({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch)
{ assert ValidTid(tid); assert shadow.Lock[ShadowableVar(x)] == tid; e := sx.R[x]; }

procedure {:yields} {:layer 0} {:refines "AtomicVarStateGetRShared"} VarStateGetRShared({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch);
procedure {:right} {:layer 1,20} AtomicVarStateGetRShared({:linear "tid"} tid:Tid, x : Var) returns (e:Epoch)
{ assert ValidTid(tid); assume sx.R[x] == SHARED; e := SHARED; }


// VCs

procedure {:yields} {:layer 0} {:refines "AtomicVCGetSize"} VCGetSize({:linear "tid" } tid: Tid, r: Shadowable) returns (i: int);
procedure {:both} {:layer 1,10} AtomicVCGetSize({:linear "tid" } tid: Tid, r: Shadowable) returns (i: int)
{
   assert ValidTid(tid);
   assert (shadow.Lock[r] == tid);
   i := VCArrayLen(shadow.VC[r]);
}

procedure {:yields} {:layer 0} {:refines "AtomicVCGetElem"} VCGetElem({:linear "tid" } tid: Tid, r: Shadowable, i: int) returns (e: Epoch);
procedure {:both} {:layer 1,20} AtomicVCGetElem({:linear "tid" } tid: Tid, r: Shadowable, i: int) returns (e: Epoch)
{
   assert ValidTid(tid);
   assert (shadow.Lock[r] == tid);
   e := VCArrayGet(shadow.VC[r], i);
}

procedure {:yields} {:layer 0} {:refines "AtomicVCGetElemShared"} VCGetElemShared({:linear "tid" } tid: Tid, x : Var) returns (e: Epoch);
procedure {:atomic} {:layer 1,20} AtomicVCGetElemShared({:linear "tid" } tid: Tid, x : Var) returns (e: Epoch)
{
   assert sx.R[x] == SHARED;
   assert ValidTid(tid);
   e := VCArrayGet(shadow.VC[ShadowableVar(x)], tid);
}

procedure {:yields} {:layer 0} {:refines "AtomicVCSetElemShared"} VCSetElemShared({:linear "tid" } tid: Tid, x : Var, e: Epoch);
procedure {:both} {:layer 1,20} AtomicVCSetElemShared({:linear "tid" } tid: Tid, x : Var, e: Epoch)
modifies shadow.VC;
{
   assert sx.R[x] == SHARED; 
   assert ValidTid(tid); 
   assert (shadow.Lock[ShadowableVar(x)] == tid); 
   shadow.VC[ShadowableVar(x)][tid] := e;
   shadow.VC[ShadowableVar(x)] := VCArraySetLen(shadow.VC[ShadowableVar(x)], max(VCArrayLen(shadow.VC[ShadowableVar(x)]),tid+1));
}

procedure {:yields} {:layer 0} {:refines "AtomicVCSetElem"} VCSetElem({:linear "tid" } tid: Tid, r: Shadowable, i: int, e: Epoch);
procedure {:both} {:layer 1,20} AtomicVCSetElem({:linear "tid" } tid: Tid, r: Shadowable, i: int, e: Epoch)
modifies shadow.VC;
{
   assert is#ShadowableVar(r) ==> sx.R[x#ShadowableVar(r)] != SHARED; 
   assert ValidTid(tid); 
   assert (shadow.Lock[r] == tid); 
   shadow.VC[r][i] := e;
   shadow.VC[r] := VCArraySetLen(shadow.VC[r], max(VCArrayLen(shadow.VC[r]),i+1));
}

procedure {:yields} {:layer 0} {:refines "AtomicVCInit"} VCInit({:linear "tid" } tid: Tid, r: Shadowable);
procedure {:both} {:layer 1,20} AtomicVCInit({:linear "tid" } tid: Tid, r: Shadowable)
modifies shadow.VC;
{
   assert ValidTid(tid); 
   assert is#ShadowableVar(r) ==> sx.R[x#ShadowableVar(r)] != SHARED; 
   assert (shadow.Lock[r] == tid); 
   shadow.VC[r] := VC.bottom();
}