

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



staload _ = "prelude/DATS/list0.dats"

staload "../SATS/libatsyn2json_cvt.sats"

// ====== to be implemented ==========



// ====== implemented below ==========


// ====== implementation ==========

implement jsonize_v1al (x) =
case+ x of
| $S1E.V1ALint (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("V1ALint")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end
| $S1E.V1ALchar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("V1ALchar")
  val () = __arr[1] := jsonize_char (__e1)
in
  JSONarray (__arr)
end
| $S1E.V1ALstring (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("V1ALstring")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $S1E.V1ALfloat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("V1ALfloat")
  val () = __arr[1] := jsonize_double (__e1)
in
  JSONarray (__arr)
end
| $S1E.V1ALerr () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("V1ALerr")
in
  JSONarray (__arr)
end

implement jsonize_v1alist(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S1E.v1al), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_v1al(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_e1xp_node (x) =
case+ x of
| $S1E.E1XPide (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPide")
  val () = __arr[1] := jsonize_symbol(__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPint (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPint")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPintrep (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPintrep")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPchar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPchar")
  val () = __arr[1] := jsonize_char (__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPstring (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPstring")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPfloat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPfloat")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPv1al (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPv1al")
  val () = __arr[1] := jsonize_v1al(__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPnone () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPnone")
in
  JSONarray (__arr)
end
| $S1E.E1XPundef () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPundef")
in
  JSONarray (__arr)
end
| $S1E.E1XPapp (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPapp")
  val () = __arr[1] := jsonize_e1xp(__e1)
  val () = __arr[2] := jsonize_location(__e2)
  val () = __arr[3] := jsonize_e1xplst(__e3)
in
  JSONarray (__arr)
end
| $S1E.E1XPfun (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPfun")
  val () = __arr[1] := jsonize_symbolist(__e1)
  val () = __arr[2] := jsonize_e1xp(__e2)
in
  JSONarray (__arr)
end
| $S1E.E1XPeval (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPeval")
  val () = __arr[1] := jsonize_e1xp(__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPlist (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPlist")
  val () = __arr[1] := jsonize_e1xplst(__e1)
in
  JSONarray (__arr)
end
| $S1E.E1XPif (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPif")
  val () = __arr[1] := jsonize_e1xp(__e1)
  val () = __arr[2] := jsonize_e1xp(__e2)
  val () = __arr[3] := jsonize_e1xp(__e3)
in
  JSONarray (__arr)
end
| $S1E.E1XPerr () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("E1XPerr")
in
  JSONarray (__arr)
end

implement jsonize_e1xp(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "e1xp_loc"
    val __v = x.e1xp_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "e1xp_node"
    val __v = x.e1xp_node
    val __value = jsonize_e1xp_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_e1xplst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S1E.e1xp), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_e1xp(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2cst (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "s2cst_get_name"
  val __v = $S2E.s2cst_get_name(x)
  val __value = jsonize_string (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "s2cst_get_sym"
  val __v = $S2E.s2cst_get_sym(x)
  val __value = jsonize_symbol(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "s2cst_get_loc"
  val __v = $S2E.s2cst_get_loc(x)
  val __value = jsonize_location(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "s2cst_get_fil"
  val __v = $S2E.s2cst_get_fil(x)
  val __value = jsonize_filename(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2cstlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2cst), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2cst(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2cstopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_s2cst(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_s2rt (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2exp_node (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2lab (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_labs2exp (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2exp(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "s2exp_srt"
    val __v = x.s2exp_srt
    val __value = jsonize_s2rt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "s2exp_node"
    val __v = x.s2exp_node
    val __value = jsonize_s2exp_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_s2explst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2exp), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2exp(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2expopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_s2exp(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_s2explstlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2explst), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2explst(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2explstopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_s2explst(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_s2lablst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2lab), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2lab(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_labs2explst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.labs2exp), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_labs2exp(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2aspdec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "s2aspdec_loc"
    val __v = x.s2aspdec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "s2aspdec_cst"
    val __v = x.s2aspdec_cst
    val __value = jsonize_s2cst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "s2aspdec_def"
    val __v = x.s2aspdec_def
    val __value = jsonize_s2exp(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_location (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "location_beg_nrow"
  val __v = $LOC.location_beg_nrow(x)
  val __value = jsonize_int (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "location_beg_ntot"
  val __v = $LOC.location_beg_ntot(x)
  val __value = jsonize_lint (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "location_end_ntot"
  val __v = $LOC.location_end_ntot(x)
  val __value = jsonize_lint (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_symbol (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "symbol_get_name"
  val __v = $SYM.symbol_get_name(x)
  val __value = jsonize_string (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_symbolist(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($SYM.symbol), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_symbol(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_symbolopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_symbol(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_token (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_i0nt(x) = 
  jsonize_token(x)

implement jsonize_i0ntopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_i0nt(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_c0har(x) = 
  jsonize_token(x)

implement jsonize_f0loat(x) = 
  jsonize_token(x)

implement jsonize_s0tring(x) = 
  jsonize_token(x)

implement jsonize_s0tringopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_s0tring(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_i0de(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "i0de_loc"
    val __v = x.i0de_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i0de_sym"
    val __v = x.i0de_sym
    val __value = jsonize_symbol(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_i0delst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($SYN.i0de), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_i0de(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2cst (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "d2cst_get_sym"
  val __v = $D2E.d2cst_get_sym(x)
  val __value = jsonize_symbol(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "d2cst_get_loc"
  val __v = $D2E.d2cst_get_loc(x)
  val __value = jsonize_location(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "d2cst_get_fil"
  val __v = $D2E.d2cst_get_fil(x)
  val __value = jsonize_filename(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)
  val __name = "d2cst_get_name"
  val __v = $D2E.d2cst_get_name(x)
  val __value = jsonize_string (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2cstlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2cst), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2cst(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2cstopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_d2cst(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_d2exp(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "d2exp_loc"
    val __v = x.d2exp_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2exp_node"
    val __v = x.d2exp_node
    val __value = jsonize_d2exp_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2exp_type"
    val __v = x.d2exp_type
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_d2explst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2exp), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2exp(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2expopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_d2exp(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_labd2exp (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_labd2explst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.labd2exp), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_labd2exp(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2exparglst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2exparg), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2exparg(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2var (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "d2var_get_sym"
  val __v = $D2E.d2var_get_sym(x)
  val __value = jsonize_symbol(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2varlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2var), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2var(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2varopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_d2var(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_cstsp (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2con_type (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2con(x) = 
  jsonize_d2con_type(x)

implement jsonize_d2conlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($STAEXP2.d2con), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2con(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2exparg (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2exparglst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2exparg), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2exparg(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d0ynq_node (x) =
case+ x of
| $SYN.D0YNQnone () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D0YNQnone")
in
  JSONarray (__arr)
end
| $SYN.D0YNQsymdot (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D0YNQsymdot")
  val () = __arr[1] := jsonize_symbol(__e1)
in
  JSONarray (__arr)
end
| $SYN.D0YNQsymcolon (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D0YNQsymcolon")
  val () = __arr[1] := jsonize_symbol(__e1)
in
  JSONarray (__arr)
end
| $SYN.D0YNQsymdotcolon (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D0YNQsymdotcolon")
  val () = __arr[1] := jsonize_symbol(__e1)
  val () = __arr[2] := jsonize_symbol(__e2)
in
  JSONarray (__arr)
end

implement jsonize_d0ynq(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "d0ynq_loc"
    val __v = x.d0ynq_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d0ynq_node"
    val __v = x.d0ynq_node
    val __value = jsonize_d0ynq_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_d2sym(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "d2sym_loc"
    val __v = x.d2sym_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2sym_qua"
    val __v = x.d2sym_qua
    val __value = jsonize_d0ynq(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2sym_sym"
    val __v = x.d2sym_sym
    val __value = jsonize_symbol(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2sym_pitmlst"
    val __v = x.d2sym_pitmlst
    val __value = jsonize_d2pitmlst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_d2symopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_d2sym(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_t2mpmarg (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_t2mpmarglst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.t2mpmarg), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_t2mpmarg(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_i2nvresstate (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_caskind (x) =
case+ x of
| $BAS.CK_case () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("CK_case")
in
  JSONarray (__arr)
end
| $BAS.CK_case_pos () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("CK_case_pos")
in
  JSONarray (__arr)
end
| $BAS.CK_case_neg () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("CK_case_neg")
in
  JSONarray (__arr)
end

implement jsonize_c2lau (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_c2laulst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.c2lau), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_c2lau(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_sc2lau (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_sc2laulst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.sc2lau), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_sc2lau(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2lab (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2lablst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2lab), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2lab(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2eff (x) =
case+ x of
| $S2E.S2EFFset (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("S2EFFset")
  val () = __arr[1] := jsonize_effset(__e1)
in
  JSONarray (__arr)
end
| $S2E.S2EFFexp (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("S2EFFexp")
  val () = __arr[1] := jsonize_s2exp(__e1)
in
  JSONarray (__arr)
end
| $S2E.S2EFFadd (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("S2EFFadd")
  val () = __arr[1] := jsonize_s2eff(__e1)
  val () = __arr[2] := jsonize_s2eff(__e2)
in
  JSONarray (__arr)
end

implement jsonize_effset_t0ype (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_effset(x) = 
  jsonize_effset_t0ype(x)

implement jsonize_s2var_type (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_s2var(x) = 
  jsonize_s2var_type(x)

implement jsonize_s2varlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2var), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2var(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_s2varopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_s2var(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_s2varlstlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2varlst), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2varlst(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_loopi2nv (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2mac_type (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_d2mac(x) = 
  jsonize_d2mac_type(x)

implement jsonize_d2maclst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2mac), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2mac(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_macsynkind (x) =
case+ x of
| $SYN.MSKencode () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("MSKencode")
in
  JSONarray (__arr)
end
| $SYN.MSKdecode () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("MSKdecode")
in
  JSONarray (__arr)
end
| $SYN.MSKxstage () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("MSKxstage")
in
  JSONarray (__arr)
end

implement jsonize_d2itm (x) =
case+ x of
| $D2E.D2ITMcst (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMcst")
  val () = __arr[1] := jsonize_d2cst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2ITMvar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMvar")
  val () = __arr[1] := jsonize_d2var(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2ITMcon (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMcon")
  val () = __arr[1] := jsonize_d2conlst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2ITMe1xp (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMe1xp")
  val () = __arr[1] := jsonize_e1xp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2ITMsymdef (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMsymdef")
  val () = __arr[1] := jsonize_symbol(__e1)
  val () = __arr[2] := jsonize_d2pitmlst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2ITMmacdef (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMmacdef")
  val () = __arr[1] := jsonize_d2mac(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2ITMmacvar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2ITMmacvar")
  val () = __arr[1] := jsonize_d2var(__e1)
in
  JSONarray (__arr)
end

implement jsonize_d2pitm (x) =
case+ x of
| $D2E.D2PITM (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2PITM")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_d2itm(__e2)
in
  JSONarray (__arr)
end

implement jsonize_d2itmlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2itm), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2itm(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2pitmlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2pitm), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2pitm(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2itmopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_d2itm(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_funclo (x) =
case+ x of
| $BAS.FUNCLOfun () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FUNCLOfun")
in
  JSONarray (__arr)
end
| $BAS.FUNCLOclo (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("FUNCLOclo")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end

implement jsonize_label_type (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_label(x) = 
  jsonize_label_type(x)

implement jsonize_d2exparg (x) =
case+ x of
| $D2E.D2EXPARGsta (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2EXPARGsta")
  val () = __arr[1] := jsonize_location(__e1)
  val () = __arr[2] := jsonize_s2exparglst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2EXPARGdyn (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2EXPARGdyn")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_location(__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
in
  JSONarray (__arr)
end

implement jsonize_d2lab_node (x) =
case+ x of
| $D2E.D2LABlab (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2LABlab")
  val () = __arr[1] := jsonize_label(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2LABind (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2LABind")
  val () = __arr[1] := jsonize_d2explst(__e1)
in
  JSONarray (__arr)
end

implement jsonize_d2eclist(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.d2ecl), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_d2ecl(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_d2ecl(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "d2ecl_loc"
    val __v = x.d2ecl_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "d2ecl_node"
    val __v = x.d2ecl_node
    val __value = jsonize_d2ecl_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_dcstkind (x) =
case+ x of
| $BAS.DCKfun () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKfun")
in
  JSONarray (__arr)
end
| $BAS.DCKval () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKval")
in
  JSONarray (__arr)
end
| $BAS.DCKpraxi () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKpraxi")
in
  JSONarray (__arr)
end
| $BAS.DCKprfun () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKprfun")
in
  JSONarray (__arr)
end
| $BAS.DCKprval () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKprval")
in
  JSONarray (__arr)
end
| $BAS.DCKcastfn () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("DCKcastfn")
in
  JSONarray (__arr)
end

implement jsonize_i2mpdec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "i2mpdec_loc"
    val __v = x.i2mpdec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_locid"
    val __v = x.i2mpdec_locid
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_cst"
    val __v = x.i2mpdec_cst
    val __value = jsonize_d2cst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_imparg"
    val __v = x.i2mpdec_imparg
    val __value = jsonize_s2varlst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_tmparg"
    val __v = x.i2mpdec_tmparg
    val __value = jsonize_s2explstlst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_tmpgua"
    val __v = x.i2mpdec_tmpgua
    val __value = jsonize_s2explstlst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "i2mpdec_def"
    val __v = x.i2mpdec_def
    val __value = jsonize_d2exp(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_funkind (x) =
case+ x of
| $BAS.FK_fn () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_fn")
in
  JSONarray (__arr)
end
| $BAS.FK_fnx () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_fnx")
in
  JSONarray (__arr)
end
| $BAS.FK_fun () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_fun")
in
  JSONarray (__arr)
end
| $BAS.FK_prfn () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_prfn")
in
  JSONarray (__arr)
end
| $BAS.FK_prfun () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_prfun")
in
  JSONarray (__arr)
end
| $BAS.FK_praxi () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_praxi")
in
  JSONarray (__arr)
end
| $BAS.FK_castfn () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("FK_castfn")
in
  JSONarray (__arr)
end

implement jsonize_s2qua(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "s2qua_svs"
    val __v = x.s2qua_svs
    val __value = jsonize_s2varlst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "s2qua_sps"
    val __v = x.s2qua_sps
    val __value = jsonize_s2explst(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_s2qualst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($S2E.s2qua), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_s2qua(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_f2undec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "f2undec_loc"
    val __v = x.f2undec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "f2undec_var"
    val __v = x.f2undec_var
    val __value = jsonize_d2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "f2undec_def"
    val __v = x.f2undec_def
    val __value = jsonize_d2exp(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "f2undec_ann"
    val __v = x.f2undec_ann
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_f2undeclst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.f2undec), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_f2undec(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_valkind (x) =
case+ x of
| $BAS.VK_val () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("VK_val")
in
  JSONarray (__arr)
end
| $BAS.VK_prval () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("VK_prval")
in
  JSONarray (__arr)
end
| $BAS.VK_val_pos () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("VK_val_pos")
in
  JSONarray (__arr)
end
| $BAS.VK_val_neg () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("VK_val_neg")
in
  JSONarray (__arr)
end

implement jsonize_v2aldec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "v2aldec_loc"
    val __v = x.v2aldec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2aldec_pat"
    val __v = x.v2aldec_pat
    val __value = jsonize_p2at(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2aldec_def"
    val __v = x.v2aldec_def
    val __value = jsonize_d2exp(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2aldec_ann"
    val __v = x.v2aldec_ann
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_v2aldeclst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.v2aldec), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_v2aldec(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_v2ardec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "v2ardec_loc"
    val __v = x.v2ardec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_knd"
    val __v = x.v2ardec_knd
    val __value = jsonize_int (__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_svar"
    val __v = x.v2ardec_svar
    val __value = jsonize_s2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_dvar"
    val __v = x.v2ardec_dvar
    val __value = jsonize_d2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_pfat"
    val __v = x.v2ardec_pfat
    val __value = jsonize_d2varopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_type"
    val __v = x.v2ardec_type
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_init"
    val __v = x.v2ardec_init
    val __value = jsonize_d2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "v2ardec_dvaropt"
    val __v = x.v2ardec_dvaropt
    val __value = jsonize_d2varopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_v2ardeclst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.v2ardec), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_v2ardec(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_prv2ardec(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "prv2ardec_loc"
    val __v = x.prv2ardec_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "prv2ardec_dvar"
    val __v = x.prv2ardec_dvar
    val __value = jsonize_d2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "prv2ardec_type"
    val __v = x.prv2ardec_type
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "prv2ardec_init"
    val __v = x.prv2ardec_init
    val __value = jsonize_d2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_prv2ardeclst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.prv2ardec), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_prv2ardec(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_filename (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "filename_get_givename"
  val __v = $FIL.filename_get_givename(x)
  val __value = jsonize_string (__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_filenv (x) =
let
  val __jp_lst = list0_nil ()

  val __name = "filenv_get_name"
  val __v = $S2E.filenv_get_name(x)
  val __value = jsonize_filename(__v)
  val __p = '(__name, __value)
  val __jp_lst = list0_cons (__p, __jp_lst)

  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_pckind (x) =
case+ x of
| $D2E.PCKcon () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("PCKcon")
in
  JSONarray (__arr)
end
| $D2E.PCKlincon () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("PCKlincon")
in
  JSONarray (__arr)
end
| $D2E.PCKfree () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("PCKfree")
in
  JSONarray (__arr)
end
| $D2E.PCKunfold () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("PCKunfold")
in
  JSONarray (__arr)
end

implement jsonize_pckindopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_pckind(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_l0ab(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "l0ab_loc"
    val __v = x.l0ab_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "l0ab_lab"
    val __v = x.l0ab_lab
    val __value = jsonize_label(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_d2ecl_node (x) =
case+ x of
| $D2E.D2Cnone () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cnone")
in
  JSONarray (__arr)
end
| $D2E.D2Clist (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Clist")
  val () = __arr[1] := jsonize_d2eclist(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Csymintr (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Csymintr")
  val () = __arr[1] := jsonize_i0delst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Csymelim (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Csymelim")
  val () = __arr[1] := jsonize_i0delst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Coverload (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Coverload")
  val () = __arr[1] := jsonize_i0de(__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_d2itmopt(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Csaspdec (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Csaspdec")
  val () = __arr[1] := jsonize_s2aspdec(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Cextype (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cextype")
  val () = __arr[1] := jsonize_string (__e1)
  val () = __arr[2] := jsonize_s2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cextval (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cextval")
  val () = __arr[1] := jsonize_string (__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cextcode (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cextcode")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_string (__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Cdatdecs (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cdatdecs")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_s2cstlst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cexndecs (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cexndecs")
  val () = __arr[1] := jsonize_d2conlst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Cdcstdecs (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cdcstdecs")
  val () = __arr[1] := jsonize_dcstkind(__e1)
  val () = __arr[2] := jsonize_d2cstlst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cimpdec (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cimpdec")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_i2mpdec(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cfundecs (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cfundecs")
  val () = __arr[1] := jsonize_funkind(__e1)
  val () = __arr[2] := jsonize_s2qualst(__e2)
  val () = __arr[3] := jsonize_f2undeclst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Cvaldecs (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cvaldecs")
  val () = __arr[1] := jsonize_valkind(__e1)
  val () = __arr[2] := jsonize_v2aldeclst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cvaldecs_rec (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cvaldecs_rec")
  val () = __arr[1] := jsonize_valkind(__e1)
  val () = __arr[2] := jsonize_v2aldeclst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cvardecs (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cvardecs")
  val () = __arr[1] := jsonize_v2ardeclst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Cprvardecs (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cprvardecs")
  val () = __arr[1] := jsonize_prv2ardeclst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Cinclude (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cinclude")
  val () = __arr[1] := jsonize_d2eclist(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Cstaload (__e1,__e2,__e3,__e4,__e5) => let
  val __arr = array0_make_elt (6, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cstaload")
  val () = __arr[1] := jsonize_symbolopt(__e1)
  val () = __arr[2] := jsonize_filename(__e2)
  val () = __arr[3] := jsonize_int (__e3)
  val () = __arr[4] := jsonize_filenv(__e4)
  val () = __arr[5] := jsonize_int (__e5)
in
  JSONarray (__arr)
end
| $D2E.D2Cdynload (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cdynload")
  val () = __arr[1] := jsonize_filename(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Clocal (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Clocal")
  val () = __arr[1] := jsonize_d2eclist(__e1)
  val () = __arr[2] := jsonize_d2eclist(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Cerrdec () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Cerrdec")
in
  JSONarray (__arr)
end

implement jsonize_d2exp_node (x) =
case+ x of
| $D2E.D2Ecst (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ecst")
  val () = __arr[1] := jsonize_d2cst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Evar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Evar")
  val () = __arr[1] := jsonize_d2var(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eint (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eint")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eintrep (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eintrep")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Ebool (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ebool")
  val () = __arr[1] := jsonize_bool (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Echar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Echar")
  val () = __arr[1] := jsonize_char (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Efloat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Efloat")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Estring (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Estring")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Ei0nt (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ei0nt")
  val () = __arr[1] := jsonize_i0nt(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Ec0har (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ec0har")
  val () = __arr[1] := jsonize_c0har(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Ef0loat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ef0loat")
  val () = __arr[1] := jsonize_f0loat(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Es0tring (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Es0tring")
  val () = __arr[1] := jsonize_s0tring(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Etop () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Etop")
in
  JSONarray (__arr)
end
| $D2E.D2Etop2 (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Etop2")
  val () = __arr[1] := jsonize_s2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eempty () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eempty")
in
  JSONarray (__arr)
end
| $D2E.D2Ecstsp (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ecstsp")
  val () = __arr[1] := jsonize_cstsp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eextval (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eextval")
  val () = __arr[1] := jsonize_s2exp(__e1)
  val () = __arr[2] := jsonize_string (__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eextfcall (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eextfcall")
  val () = __arr[1] := jsonize_s2exp(__e1)
  val () = __arr[2] := jsonize_string (__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Econ (__e1,__e2,__e3,__e4,__e5,__e6) => let
  val __arr = array0_make_elt (7, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Econ")
  val () = __arr[1] := jsonize_d2con(__e1)
  val () = __arr[2] := jsonize_location(__e2)
  val () = __arr[3] := jsonize_s2exparglst(__e3)
  val () = __arr[4] := jsonize_int (__e4)
  val () = __arr[5] := jsonize_location(__e5)
  val () = __arr[6] := jsonize_d2explst(__e6)
in
  JSONarray (__arr)
end
| $D2E.D2Esym (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Esym")
  val () = __arr[1] := jsonize_d2sym(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Efoldat (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Efoldat")
  val () = __arr[1] := jsonize_s2exparglst(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Efreeat (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Efreeat")
  val () = __arr[1] := jsonize_s2exparglst(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Etmpid (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Etmpid")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_t2mpmarglst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Elet (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elet")
  val () = __arr[1] := jsonize_d2eclist(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Ewhere (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ewhere")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2eclist(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eapplst (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eapplst")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2exparglst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eifhead (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eifhead")
  val () = __arr[1] := jsonize_i2nvresstate(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
  val () = __arr[4] := jsonize_d2expopt(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Esifhead (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Esifhead")
  val () = __arr[1] := jsonize_i2nvresstate(__e1)
  val () = __arr[2] := jsonize_s2exp(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
  val () = __arr[4] := jsonize_d2exp(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Ecasehead (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ecasehead")
  val () = __arr[1] := jsonize_caskind(__e1)
  val () = __arr[2] := jsonize_i2nvresstate(__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
  val () = __arr[4] := jsonize_c2laulst(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Escasehead (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Escasehead")
  val () = __arr[1] := jsonize_i2nvresstate(__e1)
  val () = __arr[2] := jsonize_s2exp(__e2)
  val () = __arr[3] := jsonize_sc2laulst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Elist (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elist")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_d2explst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Elst (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elst")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_s2expopt(__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Etup (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Etup")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Erec (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Erec")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_labd2explst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Eseq (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eseq")
  val () = __arr[1] := jsonize_d2explst(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eselab (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eselab")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2lablst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eptrof (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eptrof")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eviewat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eviewat")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Ederef (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ederef")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eassgn (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eassgn")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Exchng (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Exchng")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Earrsub (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Earrsub")
  val () = __arr[1] := jsonize_d2sym(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
  val () = __arr[3] := jsonize_location(__e3)
  val () = __arr[4] := jsonize_d2explst(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Earrpsz (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Earrpsz")
  val () = __arr[1] := jsonize_s2expopt(__e1)
  val () = __arr[2] := jsonize_d2explst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Earrinit (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Earrinit")
  val () = __arr[1] := jsonize_s2exp(__e1)
  val () = __arr[2] := jsonize_d2expopt(__e2)
  val () = __arr[3] := jsonize_d2explst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Eraise (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eraise")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eeffmask (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eeffmask")
  val () = __arr[1] := jsonize_s2eff(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eshowtype (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eshowtype")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Evcopyenv (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Evcopyenv")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eexist (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eexist")
  val () = __arr[1] := jsonize_s2exparg(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Elam_dyn (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elam_dyn")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_p2atlst(__e3)
  val () = __arr[4] := jsonize_d2exp(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Elaminit_dyn (__e1,__e2,__e3,__e4) => let
  val __arr = array0_make_elt (5, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elaminit_dyn")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_p2atlst(__e3)
  val () = __arr[4] := jsonize_d2exp(__e4)
in
  JSONarray (__arr)
end
| $D2E.D2Elam_met (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elam_met")
  val () = __arr[1] := jsonize_ref ()
  val () = __arr[2] := jsonize_s2explst(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Elam_sta (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Elam_sta")
  val () = __arr[1] := jsonize_s2varlst(__e1)
  val () = __arr[2] := jsonize_s2explst(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Efix (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Efix")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_d2var(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Edelay (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Edelay")
  val () = __arr[1] := jsonize_d2exp(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Eldelay (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eldelay")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_d2expopt(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Efor (__e1,__e2,__e3,__e4,__e5) => let
  val __arr = array0_make_elt (6, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Efor")
  val () = __arr[1] := jsonize_loopi2nv(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
  val () = __arr[4] := jsonize_d2exp(__e4)
  val () = __arr[5] := jsonize_d2exp(__e5)
in
  JSONarray (__arr)
end
| $D2E.D2Ewhile (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Ewhile")
  val () = __arr[1] := jsonize_loopi2nv(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
  val () = __arr[3] := jsonize_d2exp(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Eloopexn (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eloopexn")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Etrywith (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Etrywith")
  val () = __arr[1] := jsonize_i2nvresstate(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
  val () = __arr[3] := jsonize_c2laulst(__e3)
in
  JSONarray (__arr)
end
| $D2E.D2Emac (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Emac")
  val () = __arr[1] := jsonize_d2mac(__e1)
in
  JSONarray (__arr)
end
| $D2E.D2Emacsyn (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Emacsyn")
  val () = __arr[1] := jsonize_macsynkind(__e1)
  val () = __arr[2] := jsonize_d2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Emacfun (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Emacfun")
  val () = __arr[1] := jsonize_symbol(__e1)
  val () = __arr[2] := jsonize_d2explst(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eann_type (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eann_type")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_s2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eann_seff (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eann_seff")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_s2eff(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eann_funclo (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eann_funclo")
  val () = __arr[1] := jsonize_d2exp(__e1)
  val () = __arr[2] := jsonize_funclo(__e2)
in
  JSONarray (__arr)
end
| $D2E.D2Eerr () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("D2Eerr")
in
  JSONarray (__arr)
end

implement jsonize_p2at_node (x) =
case+ x of
| $D2E.P2Tany () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tany")
in
  JSONarray (__arr)
end
| $D2E.P2Tvar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tvar")
  val () = __arr[1] := jsonize_d2var(__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tcon (__e1,__e2,__e3,__e4,__e5,__e6) => let
  val __arr = array0_make_elt (7, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tcon")
  val () = __arr[1] := jsonize_pckind(__e1)
  val () = __arr[2] := jsonize_d2con(__e2)
  val () = __arr[3] := jsonize_s2qualst(__e3)
  val () = __arr[4] := jsonize_s2exp(__e4)
  val () = __arr[5] := jsonize_int (__e5)
  val () = __arr[6] := jsonize_p2atlst(__e6)
in
  JSONarray (__arr)
end
| $D2E.P2Tint (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tint")
  val () = __arr[1] := jsonize_int (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tintrep (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tintrep")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tbool (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tbool")
  val () = __arr[1] := jsonize_bool (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tchar (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tchar")
  val () = __arr[1] := jsonize_char (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tfloat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tfloat")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tstring (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tstring")
  val () = __arr[1] := jsonize_string (__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Ti0nt (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Ti0nt")
  val () = __arr[1] := jsonize_i0nt(__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tf0loat (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tf0loat")
  val () = __arr[1] := jsonize_f0loat(__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tempty () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tempty")
in
  JSONarray (__arr)
end
| $D2E.P2Trec (__e1,__e2,__e3) => let
  val __arr = array0_make_elt (4, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Trec")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_int (__e2)
  val () = __arr[3] := jsonize_labp2atlst(__e3)
in
  JSONarray (__arr)
end
| $D2E.P2Tlst (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tlst")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_p2atlst(__e2)
in
  JSONarray (__arr)
end
| $D2E.P2Trefas (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Trefas")
  val () = __arr[1] := jsonize_d2var(__e1)
  val () = __arr[2] := jsonize_p2at(__e2)
in
  JSONarray (__arr)
end
| $D2E.P2Texist (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Texist")
  val () = __arr[1] := jsonize_s2varlst(__e1)
  val () = __arr[2] := jsonize_p2at(__e2)
in
  JSONarray (__arr)
end
| $D2E.P2Tvbox (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tvbox")
  val () = __arr[1] := jsonize_d2var(__e1)
in
  JSONarray (__arr)
end
| $D2E.P2Tann (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tann")
  val () = __arr[1] := jsonize_p2at(__e1)
  val () = __arr[2] := jsonize_s2exp(__e2)
in
  JSONarray (__arr)
end
| $D2E.P2Tlist (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Tlist")
  val () = __arr[1] := jsonize_int (__e1)
  val () = __arr[2] := jsonize_p2atlst(__e2)
in
  JSONarray (__arr)
end
| $D2E.P2Terr () => let
  val __arr = array0_make_elt (1, JSONnul ())
  val () = __arr[0] := JSONstring ("P2Terr")
in
  JSONarray (__arr)
end

implement jsonize_labp2at (x) =
case+ x of
| $D2E.LABP2ATnorm (__e1,__e2) => let
  val __arr = array0_make_elt (3, JSONnul ())
  val () = __arr[0] := JSONstring ("LABP2ATnorm")
  val () = __arr[1] := jsonize_l0ab(__e1)
  val () = __arr[2] := jsonize_p2at(__e2)
in
  JSONarray (__arr)
end
| $D2E.LABP2ATomit (__e1) => let
  val __arr = array0_make_elt (2, JSONnul ())
  val () = __arr[0] := JSONstring ("LABP2ATomit")
  val () = __arr[1] := jsonize_location(__e1)
in
  JSONarray (__arr)
end

implement jsonize_p2at(x) = 
  let
    val __jp_lst = list0_nil ()

    val __name = "p2at_loc"
    val __v = x.p2at_loc
    val __value = jsonize_location(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "p2at_svs"
    val __v = x.p2at_svs
    val __value = jsonize_lstord_s2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "p2at_dvs"
    val __v = x.p2at_dvs
    val __value = jsonize_lstord_d2var(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "p2at_type"
    val __v = x.p2at_type
    val __value = jsonize_s2expopt(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)

    val __name = "p2at_node"
    val __v = x.p2at_node
    val __value = jsonize_p2at_node(__v)
    val __p = '(__name, __value)
    val __jp_lst = list0_cons (__p, __jp_lst)


    val __jp_lst = list0_reverse(__jp_lst)
    val __ret = JSONobject (__jp_lst)
  in
    __ret
  end

implement jsonize_p2atlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.p2at), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_p2at(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_p2atopt(x) = 
  (
  case+ x of
  | None () => let
    val __arr = array0_make_elt (1, JSONnul ())
    val () = __arr[0] := JSONstring ("None")
  in
    JSONarray (__arr)
  end
  | Some (__v) => let
    val __arr = array0_make_elt (2, JSONnul ())
    val () = __arr[0] := JSONstring ("Some")
    val () = __arr[1] := jsonize_p2at(__v)
  in
    JSONarray (__arr)
  end
  )

implement jsonize_labp2atlst(x) = 
  let
    val __len = list_length (x)

    val __arr = array0_make_elt (size_of_int (__len), JSONnul ())

    fun loop (arr: array0 (jsonVal), xs: List ($D2E.labp2at), n: int): void =
    case+ xs of
    | list_nil () => ()
    | list_cons (x, xs) => let
      val jx = 
        jsonize_labp2at(x)
      val () = arr[n] := jx
    in
      loop (arr, xs, n + 1)
    end
    val () = loop (__arr, x, 0)
  in
    JSONarray (__arr)
  end

implement jsonize_lstord_d2var (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end

implement jsonize_lstord_s2var (x) =
let
  val __jp_lst = list0_nil ()


  val __jp_lst = list0_reverse(__jp_lst)
  val __ret = JSONobject (__jp_lst)
in
  __ret
end