type expr = {
  descr : descr;
  loc : Location.t
}
and binop = Add | Sub | Mul | Eq | Lt 
and unop  = Fst | Snd
and descr =
| Var of string
| Int of int
| Bool of bool
| Pair of expr * expr
| Binop of binop * expr * expr
| Unop of unop * expr
| If of expr * expr * expr
| Fun of string * expr
| App of expr * expr
| Let of string * expr * expr
| Letrec of string * expr * expr

