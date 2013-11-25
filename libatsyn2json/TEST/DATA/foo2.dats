

extern fun {} knob():<> int

fun {} foo():<> int = let
in
  knob<> ()
end



implement main0() = let
  val x = 3
  implement knob<> () = x + 3
  val y = foo ()
in
  ()
end



