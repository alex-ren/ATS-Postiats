(*
** Some code used in the book PROGINATS
*)

(* ****** ****** *)

staload "acker.sats"

(* ****** ****** *)

implement
main () = () where {
//
// acker (3, 3) should return 61
//
  val () = assertloc (acker (3, 3) = 61)
} // end of [main]

(* ****** ****** *)

(* end of [test_acker2.dats] *)
