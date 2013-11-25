datatype AB = A of int | B of float

val x: int = 0

fun fact (x: int) =
if (x <= 0) then 0 else x * fact (x - 1)



