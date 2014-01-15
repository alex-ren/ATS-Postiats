
staload "prelude/DATS/integer.dats"

fun foo_val (s: string): void = ()
fun foo_var (s: &string): void = let
  val () = s := "good"
in end

implement main0 () = let
  var x1 = "abc"
  val x2 = "bcd"
  val () = foo_val x1
  val () = foo_var x1
  val () = assertloc (x1 = "good")

  val () = x1 := x2
  val () = assertloc (x2 != "good")
in end


