(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-20?? Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Zhiqiang Ren (aren AT cs DOT bu DOT edu)
// Start Time: July, 2013
//
(* ****** ****** *)

staload "./pats_hidynexp.sats"

staload "./pats_basics.sats"

(* ****** ****** *)

staload "./pats_staexp2.sats"
staload "./pats_dynexp2.sats"

(* ****** ****** *)

staload "./pats_dynexp3.sats"

(* ****** ****** *)

staload "./pats_histaexp.sats"

(* ****** ****** *)
//

(* ****** ****** *)

fun jats_print_hipat (hip: hipat): void
fun jats_prerr_hipat (hip: hipat): void
overload print with jats_print_hipat
overload prerr with jats_prerr_hipat
fun jats_fprint_hipat : fprint_type (hipat)
overload fprint with jats_fprint_hipat

fun jats_fprint_hipatlst : fprint_type (hipatlst)
overload fprint with jats_fprint_hipatlst
fun jats_fprint_labhipatlst : fprint_type (labhipatlst)
overload fprint with jats_fprint_labhipatlst

(* ****** ****** *)


(* ****** ****** *)

(* ****** ****** *)

fun jats_fprint_hidexp : fprint_type (hidexp)
fun jats_print_hidexp (x: hidexp): void
overload print with jats_print_hidexp
fun jats_prerr_hidexp (x: hidexp): void
overload prerr with jats_prerr_hidexp

fun jats_fprint_hidexplst : fprint_type (hidexplst)
fun jats_fprint_labhidexplst : fprint_type (labhidexplst)

(* ****** ****** *)

fun jats_fprint_hilab : fprint_type (hilab)
fun jats_fprint_hilablst : fprint_type (hilablst)

(* ****** ****** *)

fun jats_fprint_hidecl : fprint_type (hidecl)
fun jats_print_hidecl (x: hidecl): void
overload print with jats_print_hidecl
fun jats_prerr_hidecl (x: hidecl): void
overload prerr with jats_prerr_hidecl

fun jats_fprint_hideclist : fprint_type (hideclist)

fun jats_fprint_hifundec : fprint_type (hifundec)
fun jats_fprint_hivaldec : fprint_type (hivaldec)
fun jats_fprint_hivardec : fprint_type (hivardec)
fun jats_fprint_hiimpdec : fprint_type (hiimpdec)

(* ****** ****** *)

(* ****** ****** *)

(* end of [pats_hidynexp.sats] *)


