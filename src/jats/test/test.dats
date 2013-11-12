
extern fun foo1 (x: int, y: int): int

fun foo (x: int, y: int): int = x + 
y 

fun foo2 (x: int): int =
if x > 0 then 1
else foo2 (x - 1)


implement main0 () = let
  val a = 3
  val b = 4
  val ret = foo (a, b)
//   val () = println! ("ret is ", ret)
in
end

