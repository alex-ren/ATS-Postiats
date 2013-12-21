(* ****** ****** *)

abstype OBJ

(* ****** ****** *)

symintr println

(* ****** ****** *)

fun pow (x, n) =
(
  if n >= 1 then let
    val n2 = n / 2
  in
    if n > 2*n2 then pow (x*x, n2) * x else pow (x*x, n2)
  end else 1 // end of [if]
)

(* ****** ****** *)

val () = println ("pow(2, 10) = ", pow(2, 10))

(* ****** ****** *)

(* end of [test03.dats] *)

