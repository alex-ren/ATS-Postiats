(*
**
** Some utility functions
** for manipulating the syntax of ATS2
**
** Contributed by Hongwei Xi (gmhwxi AT gmail DOT com)
**
** Start Time: June, 2012
**
*)

(* ****** ****** *)

abstype position
abstype location

fun fprint_position (out: FILEref, x: position): void
fun fprint_location (out: FILEref, x: location): void

(* ****** ****** *)

abstype token
typedef tokenlst = List (token)
viewtypedef tokenlst_vt = List_vt (token)

(* ****** ****** *)

fun token_get_loc (x: token): location

fun fprint_token (out: FILEref, x: token): void

(* ****** ****** *)

fun token_is_eof (x: token): bool
fun token_is_comment (x: token): bool
fun token_is_extcode (x: token): bool
fun token_is_keyword (x: token): bool

fun token_is_char (x: token): bool
fun token_is_integer (x: token): bool
fun token_is_float (x: token): bool
fun token_is_string (x: token): bool

(* ****** ****** *)

fun fileref_get_tokenlst (inp: FILEref): tokenlst_vt

(* ****** ****** *)

abstype d0ecl

(* ****** ****** *)
//
// HX: global references
abstype globalref // for sta/dyn-constants
//
(* ****** ****** *)

datatype
synmark =
//
  | SMnone of ()
//
  | SMcomment of () // comment
  | SMkeyword of () // keyword
  | SMextcode of () // external code
//
  | SMstaexp of () // static expression
  | SMprfexp of () // proof expression
  | SMdynexp of () // dynamic expression
  | SMneuexp of () // neutral expression
//
  | SMscst_def of (globalref)
  | SMscst_use of (globalref)
//
  | SMscon_dec of (globalref)
  | SMscon_use of (globalref)
  | SMscon_assume of (globalref)
//
  | SMdcst_dec of (globalref)
  | SMdcst_use of (globalref)
  | SMdcst_implement of (globalref)
//
// end of [synmark]

fun fprint_synmark (out: FILEref, sm: synmark): void

(* ****** ****** *)

datatype
psynmark = PSM of
  (lint, synmark, int(*knd*)) // knd: 0/1: beg/end
// end of [psynmark]
typedef psynmarklst = List (psynmark)
viewtypedef psynmarklst_vt = List_vt (psynmark)

fun fprint_psynmark (out: FILEref, psm: psynmark): void

(* ****** ****** *)

(*
** HX-2012-06:
** synmark info for tokens
*)
fun listize_token2psynmark
  (toks: !tokenlst_vt): psynmarklst_vt
// end of [listize_token2psynmark]

(* ****** ****** *)

(*
** HX-2012-06:
** splitting psm list into two:
** beg-psm list and end-psm list
*)
fun psynmarklst_split (xs: psynmarklst_vt)
  : @(psynmarklst_vt(*beg*), psynmarklst_vt(*end*))
// end of [psynmarklst_split]

(* ****** ****** *)

(*
** HX-2012-06:
** synmark info for various syntatic entities
*)
fun fileref_get_psynmarklst
  (stadyn: int, inp: FILEref): psynmarklst_vt
// end of [fileref_get_psynmarklst]

(* ****** ****** *)

fun{} // a specific template
psynmark_process (out: FILEref, psm: psynmark): void

fun{} // this one is a generic
psynmarklst_process
  (out: FILEref, pos0: lint, psms: &psynmarklst_vt): void
// end of [psynmarklst_process]

viewtypedef
psynmarklstlst_vt = List_vt (psynmarklst_vt)
fun{} // this one is a generic
psynmarklstlst_process
  (out: FILEref, pos0: lint, psmss: &psynmarklstlst_vt): void
// end of [psynmarklstlst_process]

fun{}
fileref_psynmarklstlst_process (
  inp: FILEref
, out: FILEref
, psmss: psynmarklstlst_vt
, fputc: (char, FILEref) -<cloref1> int
) : void // end of [fileref_psynmarklstlst_process]

(* ****** ****** *)

(* end of [libatsyntax.sats] *)