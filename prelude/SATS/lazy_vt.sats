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
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
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
// Start Time: February, 2012
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy_vt.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// HX: [lazy_vt(VT)] :
// suspended computation of a linear value of viewtype VT
//
absviewtype
lazy_viewt0ype_viewtype
  (viewt@ype+) // boxed linear type // unnamed
stadef lazy_vt = lazy_viewt0ype_viewtype
//
(* ****** ****** *)
//
// HX: lazy linear streams
//
dataviewtype
stream_vt_con (a:viewt@ype+) =
  | stream_vt_nil (a) | stream_vt_cons (a) of (a, stream_vt a)
where stream_vt (a:viewt@ype) = lazy_vt (stream_vt_con a)
//
(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy_vt.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [lazy_vt.sats] *)