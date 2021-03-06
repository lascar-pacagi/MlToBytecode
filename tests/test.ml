let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in

let rec fac = fun n -> if n = 0 then 1 else n * fac (n - 1) in

let double = fun f -> fun x -> f (f x) in

let sq = fun x -> x * x in

let id = fun x -> x in 

(id 1, (id false, (id fac) 10))

(* if (1, (sq 100, 42)) = (fac 1, ((double sq) 10, 42)) then
  (true, (fib 10, fac 10))
else 
  (false, (fib 5, fac 5)) *)

(* let f = fun x -> let g = fun y -> x in g in f 1 *)