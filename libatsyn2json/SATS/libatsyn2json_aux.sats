
(* ****** ****** *)

staload S2E = "src/pats_staexp2.sats"

(* ****** ****** *)

staload LOC = "src/pats_location.sats"

(* ***** ***** *)

staload D2E = "src/pats_dynexp2.sats"

(* ***** ***** *)

staload SYM = "src/pats_symbol.sats"

(* ***** ***** *)

staload JS = "./json_simple.sats"
typedef jsonVal = $JS.jsonVal

(* ***** ***** *)


// typedef symbol = $SYM.symbol

(* ***** ***** *)

staload "./libatsyn2json_cvt_impl.sats"


(* ***** ***** *)


