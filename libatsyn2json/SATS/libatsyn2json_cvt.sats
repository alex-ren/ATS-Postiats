

(* ****** ****** *)

staload "./../SATS/json_simple.sats"

(* ****** ****** *)

staload S2E = "src/pats_staexp2.sats"

staload LEX = "src/pats_lexing.sats"

staload D2E = "src/pats_dynexp2.sats"

staload LOC = "src/pats_location.sats"

staload SYN = "src/pats_syntax.sats"

staload STAEXP2 = "src/pats_staexp2.sats"

staload BAS = "src/pats_basics.sats"

staload EFF = "src/pats_effect.sats"

staload LAB = "src/pats_label.sats"

staload S1E = "src/pats_staexp1.sats"

staload D1E = "src/pats_dynexp1.sats"

staload FIL = "src/pats_filename.sats"

staload SYM = "src/pats_symbol.sats"


(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(* anon *) = "prelude/DATS/array0.dats"

(* ****** ****** *)




// ====== to be implemented ==========


// ====================================================

fun jsonize_s2rt (x: $S2E.s2rt): jsonVal

fun jsonize_s2exp_node (x: $S2E.s2exp_node): jsonVal

fun jsonize_s2lab (x: $S2E.s2lab): jsonVal

fun jsonize_labs2exp (x: $S2E.labs2exp): jsonVal

fun jsonize_token (x: $LEX.token): jsonVal

fun jsonize_d2cst_type (x: $D2E.d2cst_type): jsonVal

fun jsonize_labd2exp (x: $D2E.labd2exp): jsonVal

fun jsonize_cstsp (x: $SYN.cstsp): jsonVal

fun jsonize_d2con_type (x: $STAEXP2.d2con_type): jsonVal

fun jsonize_s2exparg (x: $S2E.s2exparg): jsonVal

fun jsonize_d2sym (x: $D2E.d2sym): jsonVal

fun jsonize_t2mpmarg (x: $S2E.t2mpmarg): jsonVal

fun jsonize_i2nvresstate (x: $D2E.i2nvresstate): jsonVal

fun jsonize_c2lau (x: $D2E.c2lau): jsonVal

fun jsonize_sc2lau (x: $D2E.sc2lau): jsonVal

fun jsonize_d2lab (x: $D2E.d2lab): jsonVal

fun jsonize_effset_t0ype (x: $EFF.effset_t0ype): jsonVal

fun jsonize_s2var_type (x: $S2E.s2var_type): jsonVal

fun jsonize_loopi2nv (x: $D2E.loopi2nv): jsonVal

fun jsonize_d2mac_type (x: $D2E.d2mac_type): jsonVal

fun jsonize_label_type (x: $LAB.label_type): jsonVal

fun jsonize_lstord_d2var (x: $D2E.lstord($D2E.d2var)): jsonVal

fun jsonize_lstord_s2var (x: $D2E.lstord($S2E.s2var)): jsonVal

// ============ already implemented =============

fun jsonize_v1al (x: $S1E.v1al): jsonVal

fun jsonize_v1alist (x: $S1E.v1alist): jsonVal

fun jsonize_e1xp_node (x: $S1E.e1xp_node): jsonVal

fun jsonize_e1xp (x: $S1E.e1xp): jsonVal

fun jsonize_e1xplst (x: $S1E.e1xplst): jsonVal

fun jsonize_s2cst (x: $S2E.s2cst): jsonVal

fun jsonize_s2cstlst (x: $S2E.s2cstlst): jsonVal

fun jsonize_s2cstopt (x: $S2E.s2cstopt): jsonVal

fun jsonize_s2exp (x: $S2E.s2exp): jsonVal

fun jsonize_s2explst (x: $S2E.s2explst): jsonVal

fun jsonize_s2expopt (x: $S2E.s2expopt): jsonVal

fun jsonize_s2explstlst (x: $S2E.s2explstlst): jsonVal

fun jsonize_s2explstopt (x: $S2E.s2explstopt): jsonVal

fun jsonize_s2lablst (x: $S2E.s2lablst): jsonVal

fun jsonize_labs2explst (x: $S2E.labs2explst): jsonVal

fun jsonize_s2aspdec (x: $S2E.s2aspdec): jsonVal

fun jsonize_location (x: $LOC.location): jsonVal

fun jsonize_symbol (x: $SYM.symbol): jsonVal

fun jsonize_symbolist (x: $SYM.symbolist): jsonVal

fun jsonize_symbolopt (x: $SYM.symbolopt): jsonVal

fun jsonize_i0nt (x: $SYN.i0nt): jsonVal

fun jsonize_i0ntopt (x: $SYN.i0ntopt): jsonVal

fun jsonize_c0har (x: $SYN.c0har): jsonVal

fun jsonize_f0loat (x: $SYN.f0loat): jsonVal

fun jsonize_s0tring (x: $SYN.s0tring): jsonVal

fun jsonize_s0tringopt (x: $SYN.s0tringopt): jsonVal

fun jsonize_i0de (x: $SYN.i0de): jsonVal

fun jsonize_i0delst (x: $SYN.i0delst): jsonVal

fun jsonize_d2cst (x: $D2E.d2cst): jsonVal

fun jsonize_d2cstlst (x: $D2E.d2cstlst): jsonVal

fun jsonize_d2cstopt (x: $D2E.d2cstopt): jsonVal

fun jsonize_d2exp (x: $D2E.d2exp): jsonVal

fun jsonize_d2explst (x: $D2E.d2explst): jsonVal

fun jsonize_d2expopt (x: $D2E.d2expopt): jsonVal

fun jsonize_labd2explst (x: $D2E.labd2explst): jsonVal

fun jsonize_d2exparglst (x: $D2E.d2exparglst): jsonVal

fun jsonize_d2var (x: $D2E.d2var): jsonVal

fun jsonize_d2varlst (x: $D2E.d2varlst): jsonVal

fun jsonize_d2varopt (x: $D2E.d2varopt): jsonVal

fun jsonize_d2con (x: $STAEXP2.d2con): jsonVal

fun jsonize_d2conlst (x: $STAEXP2.d2conlst): jsonVal

fun jsonize_s2exparglst (x: $S2E.s2exparglst): jsonVal

fun jsonize_d2symopt (x: $D2E.d2symopt): jsonVal

fun jsonize_t2mpmarglst (x: $S2E.t2mpmarglst): jsonVal

fun jsonize_caskind (x: $BAS.caskind): jsonVal

fun jsonize_c2laulst (x: $D2E.c2laulst): jsonVal

fun jsonize_sc2laulst (x: $D2E.sc2laulst): jsonVal

fun jsonize_d2lablst (x: $D2E.d2lablst): jsonVal

fun jsonize_s2eff (x: $S2E.s2eff): jsonVal

fun jsonize_effset (x: $EFF.effset): jsonVal

fun jsonize_s2var (x: $S2E.s2var): jsonVal

fun jsonize_s2varlst (x: $S2E.s2varlst): jsonVal

fun jsonize_s2varopt (x: $S2E.s2varopt): jsonVal

fun jsonize_s2varlstlst (x: $S2E.s2varlstlst): jsonVal

fun jsonize_d2mac (x: $D2E.d2mac): jsonVal

fun jsonize_d2maclst (x: $D2E.d2maclst): jsonVal

fun jsonize_macsynkind (x: $SYN.macsynkind): jsonVal

fun jsonize_d2itm (x: $D2E.d2itm): jsonVal

fun jsonize_d2pitm (x: $D2E.d2pitm): jsonVal

fun jsonize_d2itmlst (x: $D2E.d2itmlst): jsonVal

fun jsonize_d2pitmlst (x: $D2E.d2pitmlst): jsonVal

fun jsonize_d2itmopt (x: $D2E.d2itmopt): jsonVal

fun jsonize_funclo (x: $BAS.funclo): jsonVal

fun jsonize_label (x: $LAB.label): jsonVal

fun jsonize_d2exparg (x: $D2E.d2exparg): jsonVal

fun jsonize_d2lab_node (x: $D2E.d2lab_node): jsonVal

fun jsonize_d2eclist (x: $D2E.d2eclist): jsonVal

fun jsonize_d2ecl (x: $D2E.d2ecl): jsonVal

fun jsonize_dcstkind (x: $BAS.dcstkind): jsonVal

fun jsonize_i2mpdec (x: $D2E.i2mpdec): jsonVal

fun jsonize_funkind (x: $BAS.funkind): jsonVal

fun jsonize_s2qua (x: $S2E.s2qua): jsonVal

fun jsonize_s2qualst (x: $S2E.s2qualst): jsonVal

fun jsonize_f2undec (x: $D2E.f2undec): jsonVal

fun jsonize_f2undeclst (x: $D2E.f2undeclst): jsonVal

fun jsonize_valkind (x: $BAS.valkind): jsonVal

fun jsonize_v2aldec (x: $D2E.v2aldec): jsonVal

fun jsonize_v2aldeclst (x: $D2E.v2aldeclst): jsonVal

fun jsonize_v2ardec (x: $D2E.v2ardec): jsonVal

fun jsonize_v2ardeclst (x: $D2E.v2ardeclst): jsonVal

fun jsonize_prv2ardec (x: $D2E.prv2ardec): jsonVal

fun jsonize_prv2ardeclst (x: $D2E.prv2ardeclst): jsonVal

fun jsonize_filename (x: $FIL.filename): jsonVal

fun jsonize_filenv (x: $S2E.filenv): jsonVal

fun jsonize_pckind (x: $D2E.pckind): jsonVal

fun jsonize_pckindopt (x: $D2E.pckindopt): jsonVal

fun jsonize_l0ab (x: $SYN.l0ab): jsonVal

fun jsonize_d2ecl_node (x: $D2E.d2ecl_node): jsonVal

fun jsonize_d2exp_node (x: $D2E.d2exp_node): jsonVal

fun jsonize_p2at_node (x: $D2E.p2at_node): jsonVal

fun jsonize_labp2at (x: $D2E.labp2at): jsonVal

fun jsonize_p2at (x: $D2E.p2at): jsonVal

fun jsonize_p2atlst (x: $D2E.p2atlst): jsonVal

fun jsonize_p2atopt (x: $D2E.p2atopt): jsonVal

fun jsonize_labp2atlst (x: $D2E.labp2atlst): jsonVal

