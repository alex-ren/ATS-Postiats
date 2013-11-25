

staload "../SATS/libatsyn2json_aux.sats"
staload "../SATS/libatsyn2json_cvt_impl.sats"

implement todolocation_get_string (x) = "dd"

implement todofilename_get_givename (x) = "dd"

implement todofilename_get_partname (x) = "dd"

implement todofilename_get_fullname (x) = $SYM.symbol_make_string ("dd")

implement todofilenv_get_name (x) = $FIL.filename_get_current ()



