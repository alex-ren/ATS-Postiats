(* ****** ****** *)
(*
**
** Some utility functions
** for turning ATS2 syntax trees into JSON format
**
*)
(* ****** ****** *)
(*
**
** Author: Hongwei Xi
** Authoremail: gmhwxiATgmailDOTcom
** Start Time: November, 2013
**
*)
(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload
STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

staload "src/pats_dynexp2.sats"

(* ****** ****** *)
  
staload "./../SATS/libatsyn2json.sats"

(* ****** ****** *)

staload "./../SATS/json_simple.sats"

(* ****** ****** *)

staload _(* anon *) = "prelude/DATS/array0.dats"

(* ****** ****** *)


#define isnz JSONptr_isnot_null

implement
jsonize_d2ecl
  (d2c) = let
  val lst = list0_nil ()
  val jlocation = '("d2ecl_loc", jsonize_location (d2c.d2ecl_loc))
  val lst = list0_cons (jlocation, lst)

in
//
case+ d2c.d2ecl_node of
| _ => let
in
  JSONobject (lst)
end

(*
| _ => let
    val () = (
      prerrln! ("jsonize_d2ecl: d2c0 = ", d2c0)
    ) (* end of [val] *)
    val ((*void*)) = assertloc (false)
  in
    exit (1)
  end // end of [_]
*)
//
end // end of [jsonize_d2ecl]
  
(* ****** ****** *)

(* ****** ****** *)

implement
jsonize_d2eclist
  (d2cs) = let
  val len = list_length (d2cs)
  val arr = array0_make_elt (size_of_int (len), JSONnul ())

  fun loop (arr: array0 (jsonVal), d2cs: d2eclist, n: int): void =
  case+ d2cs of
  | list_nil () => ()
  | list_cons (d2c, d2cs) => let
    val jd2c = jsonize_d2ecl (d2c)
    val () = arr[n] := jd2c
  in
    loop (arr, d2cs, n + 1)
  end
  val () = loop (arr, d2cs, 0)
in
  JSONarray (arr)
end
    

(* ****** ****** *)

(* end of [libatsyn2json_d2ecl.dats] *)












