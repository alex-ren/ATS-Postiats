

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



staload "../SATS/libatsyn2json_cvt.sats"

// ====== to be implemented ==========

implement jsonize_s2rt (x) = 
jsonize_string ("s2rt is not supported.")

implement jsonize_s2exp_node (x) = 
jsonize_string ("s2exp_node is not supported.")

implement jsonize_s2lab (x) = 
jsonize_string ("s2lab is not supported.")

implement jsonize_labs2exp (x) = 
jsonize_string ("labs2exp is not supported.")

implement jsonize_token (x) = 
jsonize_string ("token is not supported.")

implement jsonize_d2cst_type (x) = 
jsonize_string ("d2cst_type is not supported.")

implement jsonize_labd2exp (x) = 
jsonize_string ("labd2exp is not supported.")

implement jsonize_cstsp (x) = 
jsonize_string ("cstsp is not supported.")

implement jsonize_d2con_type (x) = 
jsonize_string ("d2con_type is not supported.")

implement jsonize_s2exparg (x) = 
jsonize_string ("s2exparg is not supported.")

implement jsonize_d2sym (x) = 
jsonize_string ("d2sym is not supported.")

implement jsonize_t2mpmarg (x) = 
jsonize_string ("t2mpmarg is not supported.")

implement jsonize_i2nvresstate (x) = 
jsonize_string ("i2nvresstate is not supported.")

implement jsonize_c2lau (x) = 
jsonize_string ("c2lau is not supported.")

implement jsonize_sc2lau (x) = 
jsonize_string ("sc2lau is not supported.")

implement jsonize_d2lab (x) = 
jsonize_string ("d2lab is not supported.")

implement jsonize_effset_t0ype (x) = 
jsonize_string ("effset_t0ype is not supported.")

implement jsonize_s2var_type (x) = 
jsonize_string ("s2var_type is not supported.")

implement jsonize_loopi2nv (x) = 
jsonize_string ("loopi2nv is not supported.")

implement jsonize_d2mac_type (x) = 
jsonize_string ("d2mac_type is not supported.")

implement jsonize_label_type (x) = 
jsonize_string ("label_type is not supported.")

implement jsonize_lstord_d2var (x) = 
jsonize_string ("lstord_d2var is not supported.")

implement jsonize_lstord_s2var (x) = 
jsonize_string ("lstord_s2var is not supported.")



