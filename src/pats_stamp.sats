(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-20?? Hongwei Xi, Boston University
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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Start Time: May, 2011
//
(* ****** ****** *)

staload "pats_basics.sats"
staload "pats_counter.sats"

(* ****** ****** *)

abst@ype stamp_t0ype = count
typedef stamp = stamp_t0ype

(* ****** ****** *)

fun lt_stamp_stamp (x1: stamp, x2: stamp):<> bool
overload < with lt_stamp_stamp

fun lte_stamp_stamp (x1: stamp, x2: stamp):<> bool
overload <= with lte_stamp_stamp

fun eq_stamp_stamp (x1: stamp, x2: stamp):<> bool
overload = with eq_stamp_stamp

fun neq_stamp_stamp (x1: stamp, x2: stamp):<> bool
overload <> with neq_stamp_stamp

fun compare_stamp_stamp (x1: stamp, x2: stamp):<> Sgn
overload compare with compare_stamp_stamp

(* ****** ****** *)

fun fprint_stamp : fprint_type (stamp)

(* ****** ****** *)
//
fun
s2rtdat_stamp_make (): stamp
//
fun s2cst_stamp_make (): stamp
//
fun s2var_stamp_make (): stamp
fun s2Var_stamp_make (): stamp
//
(* ****** ****** *)
//
fun d2con_stamp_make (): stamp
//
fun d2cst_stamp_make (): stamp
//
fun d2mac_stamp_make (): stamp
//
fun d2var_stamp_make (): stamp
//
(* ****** ****** *)

(* end of [pats_stamp.sats] *)