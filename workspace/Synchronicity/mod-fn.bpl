

function MOD(x:int,y:int): int;

axiom (forall x : int :: { MOD(x, 512) } (0 <= x && x < 512 ==> MOD(x, 512) == x));
axiom MOD(512, 512) == 0;

axiom (forall x:int, y:int :: { MOD(512 + MOD(y + 1, 512) - x, 512) } { MOD(512 + y - x, 512) }
     0 <= x && x < 512 &&
     0 <= y && y < 512 &&
     MOD(y + 1, 512) != x
  ==> MOD(512 + y - x, 512) + 1 == MOD(512 + MOD(y + 1, 512) - x, 512));




procedure  f()
{
 var x : int;
 var y : int;
 var len : int;
 assume 0 <= x && x < 512;
 assume 0 <= y && y < 512;

 //assert MOD(x + 1, 512) == x + 1;

 //assert MOD(x + 2, 512) == x + 2;
 assume MOD(y + 1, 512) != x;
 assert MOD(512 + (y - x), 512) + 1 == MOD(512 + (MOD(y + 1, 512) - x), 512);
}

