(*
** Some print functions to faciliate testing
*)

(* ****** ****** *)

staload "contrib/atshwxi/testing/fprint.sats"

(* ****** ****** *)

implement{a}
fprint_list0_sep
  (out, xs, sep) = let
  val xs = __cast (xs) where {
    extern castfn __cast (xs: list0(a)): List(a)
  } // end of [val]
in
  fprint_list_sep<a> (out, xs, sep)
end // end of [fprint_list0_sep]

(* ****** ****** *)

implement{a}
fprint_list_sep
  (out, xs, sep) = let
  val xs = __cast (xs) where {
    extern castfn __cast (xs: List (a)): List_vt (a)
  } // end of [val]
  val () = fprint_list_vt_sep<a> (out, xs, sep)
  val () = __free (xs) where {
    extern castfn __free (xs: List_vt (a)): void
  } // end of [val]
in
  // nothing
end // end of [fprint_list_sep]

(* ****** ****** *)

implement{a}
fprint_list_vt_sep
  (out, xs, sep) = let
  fun loop (
    xs: !List_vt (a), notbeg: bool
  ) :<cloref1> void =
    case+ xs of
    | list_vt_cons (x, xs) => let
        val () = if notbeg then fprint_string (out, sep)
        val () = fprint_elt<a> (out, x)
      in
        loop (xs, true)
      end // end of [list_cons]
    | list_vt_nil () => ()
  // end of [loop]
in
  loop (xs, false(*notbeg*))
end // end of [fprint_list_vt_sep]

(* ****** ****** *)
//
// HX-2012-05:
// Compiling this is a challenge right now!
//
implement{a}
fprint_listlist_sep
  (out, xss, sep1, sep2) = let
//
implement
fprint_elt<List(a)>
  (out, xs) = fprint_list_sep<a> (out, xs, sep1)
// end of [fprint_elt]
//
in
//
fprint_list_sep<List(a)> (out, xss, sep2)
//
end // end of [fprint_listlist_sep]

(* ****** ****** *)

implement{a}
fprint_array_sep
  (out, A, n, sep) = let
//
prval () = lemma_array_v_param (view@(A))
//
fun loop
  {l:addr}
  {n:nat} .<n>. (
  pf: !array_v (a, l, n)
| out: FILEref
, p: ptr l, n: size_t n, sep: string, notbeg: bool
) : void =
  if n > 0 then let
    prval (pf1, pf2) = array_v_uncons (pf)
    val () = if notbeg then fprint_string (out, sep)
    val () = fprint_elt<a> (out, !p)
    val () = loop (pf2 | out, p+sizeof<a>, pred(n), sep, true)
    prval () = pf := array_v_cons (pf1, pf2)
  in
    // nothing
  end // end of [if]
(* end of [loop] *)
//
in
  loop (view@(A) | out, addr@(A), n, sep, false(*notbeg*))
end // end of [fprint_array_sep]

(* ****** ****** *)

implement{a}
fprint_arrayptr_sep
  (out, A, n, sep) = () where {
  val p = ptrcast (A)
  prval pf = arrayptr_takeout (A)
  val () = fprint_array_sep<a> (out, !p, n, sep)
  prval () = arrayptr_addback (pf | A)
} // end of [fprint_arrayptr_sep]

(* ****** ****** *)

(* end of [fprint.dats] *)