(* ****** ****** *)

abstype OBJ

(* ****** ****** *)

symintr println

(* ****** ****** *)

extern
fun acker (OBJ, OBJ): OBJ

(* ****** ****** *)

implement
acker (m, n) =
(
if m > 0
  then
    if n > 0
      then acker (m-1, acker (m, n-1)) else acker (m-1, 1)
    // end of [if]
  else n+1
// end of [if]
)

(* ****** ****** *)

val () = println ("acker(3, 3) = ", acker(3, 3))

(* ****** ****** *)

(* end of [test02.dats] *)
