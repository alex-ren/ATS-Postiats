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
// Author: Hongwei Xi (gmhwxi AT gmail DOT com)
// Start Time: October, 2012
//
(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"
staload _(*anon*) = "prelude/DATS/unsafe.dats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload "./pats_basics.sats"

(* ****** ****** *)

staload LOC = "./pats_location.sats"
overload print with $LOC.print_location

(* ****** ****** *)

staload "./pats_staexp2.sats"
staload "./pats_dynexp2.sats"

(* ****** ****** *)

staload "./pats_trans2_env.sats"

(* ****** ****** *)

staload "./pats_trans3.sats"

(* ****** ****** *)

staload "./pats_histaexp.sats"
staload "./pats_hidynexp.sats"

(* ****** ****** *)

staload "./pats_typerase.sats"

(* ****** ****** *)

staload "./pats_ccomp.sats"

(* ****** ****** *)

assume ccomp_instrlst_type = instrlst

(* ****** ****** *)

extern
fun hisaspdec_ccomp
  (env: !ccompenv, hid0: hidecl): primdec
// end of [hidsaspdec_ccomp]

extern
fun hiextcode_ccomp
  (env: !ccompenv, hid0: hidecl): primdec
// end of [hidextcode_ccomp]

extern
fun hidatdecs_ccomp
  (env: !ccompenv, hid0: hidecl): primdec
// end of [hidatdecs_ccomp]
extern
fun hiexndecs_ccomp
  (env: !ccompenv, hid0: hidecl): primdec
// end of [hiexndecs_ccomp]

(* ****** ****** *)

extern
fun hifundeclst_ccomp
(
  env: !ccompenv, level: int
, knd: funkind, decarg: s2qualst, hfds: hifundeclst
) : void // end of [hifundeclst_ccomp]

(* ****** ****** *)

extern
fun hivaldeclst_ccomp
(
  env: !ccompenv, level: int, knd: valkind, hvds: hivaldeclst
) : instrlst // end of [hivaldeclst_ccomp]

extern
fun hivaldeclst_ccomp_rec
(
  env: !ccompenv, level: int, knd: valkind, hvds: hivaldeclst
) : instrlst // end of [hivaldeclst_ccomp_rec]

extern
fun hivardeclst_ccomp
  (env: !ccompenv, level: int, hvds: hivardeclst): instrlst
// end of [hivardeclst_ccomp]

(* ****** ****** *)

implement
hidecl_ccomp
  (env, hid0) = let
//
val loc0 = hid0.hidecl_loc
//
in
//
case+ hid0.hidecl_node of
//
| HIDnone () => primdec_none (loc0)
//
| HIDlist (hids) => let
    val pmds =
      hideclist_ccomp (env, hids) in primdec_list (loc0, pmds)
    // end of [val]
  end // end of [HIDlist]
//
| HIDsaspdec _ => hisaspdec_ccomp (env, hid0)
//
| HIDextcode _ => hiextcode_ccomp (env, hid0)
//
| HIDdatdecs _ => hidatdecs_ccomp (env, hid0)
| HIDexndecs _ => hiexndecs_ccomp (env, hid0)
//
| HIDdcstdecs
    (knd, d2cs) => primdec_none (loc0)
  // end of [HIDdcstdecs]
//
| HIDimpdec
    (knd, imp) => let
    val d2c = imp.hiimpdec_cst
    val lvl0 = the_d2varlev_get ()
    val () = hiimpdec_ccomp (env, lvl0, imp)
  in
    primdec_impdec (loc0, imp)
  end // end of [HIDimpdec]
//
| HIDfundecs
    (knd, decarg, hfds) => let
    val lvl0 = the_d2varlev_get ()
    val () =
      hifundeclst_ccomp (env, lvl0, knd, decarg, hfds)
    // end of [val]
  in
    primdec_fundecs (loc0, knd, decarg, hfds)
  end // end of [HIDfundecs]
//
| HIDvaldecs
    (knd, hvds) => let
    val lvl0 = the_d2varlev_get ()
    val inss = hivaldeclst_ccomp (env, lvl0, knd, hvds)
  in
    primdec_valdecs (loc0, knd, hvds, inss)
  end // end of [HIDvaldecs]
| HIDvaldecs_rec
    (knd, hvds) => let
    val lvl0 = the_d2varlev_get ()
    val inss = hivaldeclst_ccomp_rec (env, lvl0, knd, hvds)
  in
    primdec_valdecs_rec (loc0, knd, hvds, inss)
  end // end of [HIDvaldecs_rec]
//
| HIDvardecs
    (hvds) => let
    val lvl0 = the_d2varlev_get ()
    val inss = hivardeclst_ccomp (env, lvl0, hvds)
  in
    primdec_vardecs (loc0, hvds, inss)
  end // end of [HIDvardecs]
//
| HIDinclude
    (hids) => let
// (*
    val () = println! ("hidecl_ccomp: HIDinclude: loc0 = ", loc0)
    val () = println! ("hidecl_ccomp: HIDinclude: hid0 = ", hid0)
// *)
    val pmds = hideclist_ccomp (env, hids)
  in
    primdec_include (loc0, pmds)
  end // end of [HIDinclude]
//
| HIDstaload
  (
    fil, flag, fenv, loaded
  ) => let
(*
    val () = println! ("hidecl_ccomp: HIDstaload: loc0 = ", loc0)
    val () = println! ("hidecl_ccomp: HIDstaload: hid0 = ", hid0)
*)
    val () = the_staloadlst_add (hid0)
    val () = ccompenv_add_staload (env, fenv)
  in
    primdec_staload (loc0, fenv)
  end // end of [HIDstaload]
//
| HIDlocal
  (
    hids_head, hids_body
  ) => let
    val pmds_head = hideclist_ccomp (env, hids_head)
    val pmds_body = hideclist_ccomp (env, hids_body)
  in
    primdec_local (loc0, pmds_head, pmds_body)
  end // end of [HIDlocal]
//
(*
| _ => let
    val () = println! ("hidecl_ccomp: loc0 = ", loc0)
    val () = println! ("hidecl_ccomp: hid0 = ", hid0)
  in
    exitloc (1)
  end // end of [_]
*)
//
end // end of [hidecl_ccomp]

(* ****** ****** *)

implement
hideclist_ccomp
  (env, hids) = let
//
fun loop (
  env: !ccompenv
, hids: hideclist
, pmds: &primdeclst_vt? >> primdeclst_vt
) : void = let
in
//
case+ hids of
| list_cons
    (hid, hids) => let
    val pmd =
      hidecl_ccomp (env, hid)
    val () = pmds := list_vt_cons {..}{0} (pmd, ?)
    val list_vt_cons (_, !p_pmds) = pmds
    val () = loop (env, hids, !p_pmds)
    val () = fold@ (pmds)
  in
    // nothing
  end // end of [list_cons]
| list_nil () => let
    val () = pmds := list_vt_nil () in (*nothing*)
  end // end of [list_nil]
//
end // end of [loop]
//
var pmds: primdeclst_vt
val () = loop (env, hids, pmds)
//
in
//
list_of_list_vt (pmds)
//
end // end of [hideclist_ccomp]

(* ****** ****** *)

implement
hideclist_ccomp0
  (hids) = let
//
val env = ccompenv_make ()
val pmds = hideclist_ccomp (env, hids)
val () = ccompenv_free (env)
//
in
  pmds
end // end of [hideclist_ccomp0]

(* ****** ****** *)

implement
hisaspdec_ccomp
  (env, hid0) = let
//
val loc0 = hid0.hidecl_loc
val-HIDsaspdec (d2c) = hid0.hidecl_node
val () = the_saspdeclst_add (hid0)
//
in
  primdec_saspdec (loc0, d2c)
end // end of [hisaspdec_ccomp]

(* ****** ****** *)

implement
hiextcode_ccomp
  (env, hid0) = let
//
val loc0 = hid0.hidecl_loc
val () = the_extcodelst_add (hid0)
//
in
  primdec_none (loc0)
end // end of [hiextcod_ccomp]

(* ****** ****** *)

implement
hidatdecs_ccomp
  (env, hid0) = let
//
val loc0 = hid0.hidecl_loc
val-HIDdatdecs (knd, s2cs) = hid0.hidecl_node
val isprf = test_prfkind (knd)
//
in
//
if isprf
  then primdec_none (loc0)
  else primdec_datdecs (loc0, s2cs)
// end of [if]
//
end // end of [hidatdecs_ccomp]

(* ****** ****** *)

implement
hiexndecs_ccomp
  (env, hid0) = let
//
val loc0 = hid0.hidecl_loc
val-HIDexndecs (d2cs) = hid0.hidecl_node
//
in
  primdec_exndecs (loc0, d2cs)
end // end of [hiexndecs_ccomp]

(* ****** ****** *)

local

fun auxinit
  {n:nat} .<n>.
(
  env: !ccompenv
, level: int, decarg: s2qualst, hfds: list (hifundec, n)
) : list_vt (funlab, n) = let
in
//
case+ hfds of
| list_cons
    (hfd, hfds) => let
    val loc = hfd.hifundec_loc
    val d2v = hfd.hifundec_var
    val () = d2var_set_level (d2v, level)
    val-Some (hse) = d2var_get2_hisexp (d2v)
    val fcopt = None_vt() // HX: determined by [hse]
//
    val tmplev = ccompenv_get_tmplevel (env)
//
    val fl = funlab_make_dvar_type (d2v, hse, fcopt)
//
    val pmv =
    (
      if tmplev = 0
        then primval_make_funlab (loc, fl)
        else primval_make_d2vfunlab (loc, d2v, fl)
    ) : primval // end of [val]
//
    val () = ccompenv_add_vbindmapenvall (env, d2v, pmv)
//
    val istmp =
    (
      if tmplev > 0 then true else list_is_cons (decarg)
    ) : bool // end of [val]
    val () = if istmp then funlab_set_tmpknd (fl, 1)
//
    val () =
    (
      case+ decarg of
      | list_cons _ => ccompenv_add_fundec (env, hfd)
      | list_nil () => ()
    ) : void // end of [val]
//
    val fls = auxinit (env, level, decarg, hfds)
  in
    list_vt_cons (fl, fls)
  end // end of [list_cons]
| list_nil () => list_vt_nil ()
//
end (* end of [auxinit] *)

fun auxmain
  {n:nat} .<n>.
(
  env: !ccompenv
, knd: funkind, decarg: s2qualst
, hfds: list (hifundec, n), flabs: list_vt (funlab, n)
) : void = let
in
//
case+ hfds of
| list_cons
    (hfd, hfds) => let
    val loc = hfd.hifundec_loc
    val d2v = hfd.hifundec_var
    val imparg = hfd.hifundec_imparg
    val hde_def = hfd.hifundec_def
    val-HDElam (hips_arg, hde_body) = hde_def.hidexp_node
    val+~list_vt_cons (flab, flabs) = flabs
    val tmparg = list_nil(*s2ess*) // HX: matches everything?
    val ins = instr_funlab (loc, flab)
    val prolog = list_sing (ins)
//
    val istmp = list_is_cons (decarg)
    val () = if istmp then ccompenv_inc_tmplevel (env)
    val fent =
      hidexp_ccomp_funlab_arg_body (
      env, flab, imparg, tmparg, prolog, loc, hips_arg, hde_body
    ) // end of [fcall] // end of [val]
    val () = if istmp then ccompenv_dec_tmplevel (env)
//
    val () =
      hifundec_set_funlabopt (hfd, Some (flab))
    // end of [val]
    val () = funlab_set_funent (flab, Some (fent))
(*
    val () = println! ("auxmain: fent=", fent)
*)
  in
    auxmain (env, knd, decarg, hfds, flabs)
  end // end of [list_vt_cons]
| list_nil () => let
    val+~list_vt_nil () = flabs in (*nothing*)
  end // end of [list_nil]
//
end (* end of [auxmain] *)

in (* in of [local] *)

implement
hifundeclst_ccomp (
  env, level, knd, decarg, hfds
) = let
  val flabs =
    auxinit (env, level, decarg, hfds)
  // end of [val]
  val () = the_funlablst_addlst ($UN.castvwtp1{funlablst}(flabs))
in
  auxmain (env, knd, decarg, hfds, flabs)
end // end of [hifundeclst_ccomp]

end // end of [local]

(* ****** ****** *)

local

fun aux
(
  env: !ccompenv
, res: !instrseq
, level: int, knd: valkind, hvd: hivaldec
) : void = let
  val loc = hvd.hivaldec_loc
  val hde_def = hvd.hivaldec_def
  val pmv_def = hidexp_ccomp (env, res, hde_def)
  val hip = hvd.hivaldec_pat
  val fail = (
    case+ knd of
    | VK_val_pos () => PTCKNTnone () | _ => PTCKNTcaseof_fail (loc)
  ) : patckont // end of [val]
  val () = hipatck_ccomp (env, res, fail, hip, pmv_def)
  val () = himatch_ccomp (env, res, level, hip, pmv_def)
in
  // nothing
end // end of [aux]

fun auxlst
(
  env: !ccompenv
, res: !instrseq
, level: int, knd: valkind, hvds: hivaldeclst
) : void = let
in
//
case+ hvds of
| list_cons (hvd, hvds) => let
    val () = aux (env, res, level, knd, hvd)
    val () = auxlst (env, res, level, knd, hvds)
  in
    // nothing
  end // end of [list_cons]
| list_nil () => ()
//
end // end of [auxlst]

in (* in of [local] *)

implement
hivaldeclst_ccomp
(
  env, level, knd, hvds
) = let
//
var res
  : instrseq = instrseq_make_nil ()
val () = auxlst (env, res, level, knd, hvds)
//
in
  instrseq_get_free (res)
end // end of [hivaldeclst_ccomp]

end // end of [local]

(* ****** ****** *)

local

fun auxinit
  {n:nat} .<n>.
(
  env: !ccompenv
, res: !instrseq
, level: int
, hvds: list (hivaldec, n)
) : list_vt (tmpvar, n) = let
in
//
case+ hvds of
| list_cons
    (hvd, hvds) => let
    val hip = hvd.hivaldec_pat
    val loc = hip.hipat_loc
    val hse = hip.hipat_type
    val tmp = tmpvar_make (loc, hse)
    val () = instrseq_add_tmpdec (res, loc, tmp)
    val pmv = primval_tmp (loc, hse, tmp)
    val () = himatch_ccomp (env, res, level, hip, pmv)
    val tmps = auxinit (env, res, level, hvds)
  in
    list_vt_cons (tmp, tmps)
  end // end of [list_cons]
| list_nil () => list_vt_nil ()
//
end // end of [auxinit]

fun
auxmain{n:nat}
(
  env: !ccompenv
, res: !instrseq
, hvds: list (hivaldec, n)
, tmps: list_vt (tmpvar, n)
) : void = let
in
//
case+ hvds of
| list_cons
    (hvd, hvds) => let
    val hde_def = hvd.hivaldec_def
    val+~list_vt_cons (tmp, tmps) = tmps
    val () = hidexp_ccomp_ret (env, res, tmp, hde_def)
  in
    auxmain (env, res, hvds, tmps)
  end // end of [list_cons]
| list_nil () => let
    val+~list_vt_nil () = tmps in (*nothing*)
  end // end of [list_nil]
//
end // end of [auxmain]

in (* in of [local] *)

implement
hivaldeclst_ccomp_rec
  (env, level, knd, hvds) = let
//
var res: instrseq = instrseq_make_nil ()
val tmps = auxinit (env, res, level, hvds)
val () = auxmain (env, res, hvds, tmps)
//
in
  instrseq_get_free (res)
end // end of [hivaldeclst_ccomp_rec]

end // end of [local]

(* ****** ****** *)

local

fun aux
(
  env: !ccompenv
, res: !instrseq
, level: int, hvd: hivardec
) : void = let
//
val loc = hvd.hivardec_loc
val d2v = hvd.hivardec_dvar_ptr
val d2vw = hvd.hivardec_dvar_view
val loc_d2v = d2var_get_loc (d2v)
val () = d2var_set_level (d2v, level)
val s2at = d2var_get_type_some (loc_d2v, d2vw)
val-S2Eat (s2e_elt, _) = s2at.s2exp_node
val hse_elt = s2exp_tyer_shallow (loc_d2v, s2e_elt)
val tmp = tmpvar_make_ref (loc_d2v, hse_elt)
//
val () = instrseq_add_tmpdec (res, loc_d2v, tmp)
//
val () = 
(
case+
  hvd.hivardec_ini of
| Some (hde) => hidexp_ccomp_ret (env, res, tmp, hde)
| None ((*void*)) => ()
) : void // end of [val]
//
val pmv = primval_tmpref (loc, hse_elt, tmp)
val () = ccompenv_add_vbindmapenvall (env, d2v, pmv)
//
in
  // nothing
end // end of [aux]

fun auxlst
(
  env: !ccompenv
, res: !instrseq
, level: int, hvds: hivardeclst
) : void = let
in
//
case+ hvds of
| list_cons (hvd, hvds) => let
    val () = aux (env, res, level, hvd)
    val () = auxlst (env, res, level, hvds)
  in
    // nothing
  end // end of [list_cons]
| list_nil () => ()
//
end // end of [auxlst]

in (* in of [local] *)

implement
hivardeclst_ccomp
  (env, level, hvds) = let
//
var res
  : instrseq = instrseq_make_nil ()
val () = auxlst (env, res, level, hvds)
//
in
  instrseq_get_free (res)
end // end of [hivardeclst_ccomp]

end // end of [local]

(* ****** ****** *)

local

fun auxlam
(
  env: !ccompenv
, loc0: location
, d2c: d2cst
, imparg: s2varlst
, tmparg: s2explstlst
, hde0: hidexp
) : funlab = flab where
{
//
val loc_fun = hde0.hidexp_loc
val hse_fun = hde0.hidexp_type
val fcopt = d2cst_get2_funclo (d2c)
//
val-HDElam (hips_arg, hde_body) = hde0.hidexp_node
//
val flab =
  funlab_make_dcst_type (d2c, hse_fun, fcopt)
//
val tmplev = ccompenv_get_tmplevel (env)
val () =
  if tmplev > 0 then funlab_set_tmpknd (flab, 1)
// end of [val]
//
val pmv_lam = primval_make_funlab (loc0, flab)
//
val fent = let
  val ins =
    instr_funlab (loc0, flab)
  // end of [val]
  val prolog = list_sing (ins)
in
  hidexp_ccomp_funlab_arg_body
    (env, flab, imparg, tmparg, prolog, loc_fun, hips_arg, hde_body)
  // end of [hidexp_ccomp_funlab_arg_body]
end // end of [val]
//
val () = the_funlablst_add (flab)
val () = funlab_set_funent (flab, Some (fent))
//
val () = println! ("hiimpdec_ccomp: auxlam: fent = ", fent)
//
} // end of [auxlam]

fun auxmain
(
  env: !ccompenv
, loc0: location
, d2c: d2cst
, imparg: s2varlst
, tmparg: s2explstlst
, hde0: hidexp
) : funlab = let
//
val hse0 = hde0.hidexp_type
//
in
//
case+
  hde0.hidexp_node of
//
| HDElam _ =>
  (
    auxlam (env, loc0, d2c, imparg, tmparg, hde0)
  ) // end of [HDElam]
| HDEcst (d2c) => let
    val fcopt = d2cst_get2_funclo (d2c)
  in
    funlab_make_dcst_type (d2c, hse0, fcopt)
  end // end of [HDEcst]
| HDEvar (d2v) => let
    val fcopt = d2var_get2_funclo (d2v)
  in
    funlab_make_dvar_type (d2v, hse0, fcopt)
  end // end of [HDEvar]
//
| HDEtmpcst (d2c, t2mas) => let
    val fcopt = d2cst_get2_funclo (d2c)
  in
    funlab_make_tmpcst_type (d2c, t2mas, hse0, fcopt)
  end // end of [HDEtmpcst]
//
| _ => let
    val (
    ) = (
      println! ("hiimpdec_ccomp: auxmain: hde0 = ", hde0)
    ) // end of [val]
  in
    exitloc (1)
  end // end of [_]
//
end // end of [auxmain]

in (* in of [local] *)

implement
hiimpdec_ccomp
(
  env, level, imp
) = let
//
val d2c = imp.hiimpdec_cst
val knd = d2cst_get_kind (d2c)
//
val () =
(
  println! ("hiimpdec_ccomp: d2c = ", d2c)
)
//
in
//
case+ 0 of
(*
| _ when
    dcstkind_is_castfn knd => ()
*)
| _ when
    dcstkind_is_fun (knd) => let
    val loc0 = imp.hiimpdec_loc
    val imparg = imp.hiimpdec_imparg
    val tmparg = imp.hiimpdec_tmparg
    val hde_def = imp.hiimpdec_def
//
    val istmp = list_is_cons (tmparg)
//
    val () =
      if istmp then ccompenv_add_impdec (env, imp)
    // end of [val]
//
    val () = if istmp then ccompenv_inc_tmplevel (env)
    val flab = auxmain (env, loc0, d2c, imparg, tmparg, hde_def)
    val () = if istmp then ccompenv_dec_tmplevel (env)
//
    val p = hiimpdec_set_funlabopt (imp, Some (flab))
  in
    // nothing
  end // end of [if]
//
| _ (*non-fun*) => let
    var res
      : instrseq = instrseq_make_nil ()
    val pmv = hidexp_ccomp (env, res, imp.hiimpdec_def)
    val () = instrseq_add_dcstdef (res, imp.hiimpdec_loc, d2c, pmv)
    val inss = instrseq_get_free (res)
    val () = hiimpdec_set_instrlstopt (imp, Some (inss))
  in
    // nothing
  end // end of [non-fun]
//
end // end of [hiimpdec_ccomp]

end // end of [local]

(* ****** ****** *)

implement
hiimpdec_ccomp_if
  (env, level, imp) = let
  val opt = hiimpdec_get_funlabopt (imp)
in
//
case+ opt of
| Some _ => () | None _ => hiimpdec_ccomp (env, level, imp)
//
end // end of [hiimpdec_ccomp_if]

(* ****** ****** *)

(* end of [pats_ccomp_decl.dats] *)
