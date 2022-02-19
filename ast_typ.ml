type typ =
| Tint 
| Tbool
| Tarrow of typ * typ 
| Tproduct of typ * typ 
| Tvar of tvar 

and tvar = { id : int; mutable def : typ option }

type texpr = {
  tdescr : tdescr;
  typ : typ
}
and tbinop = TAdd | TSub | TMul | TEq | TLt 
and tunop  = TFst | TSnd
and tdescr =
| TVar of string
| TInt of int
| TBool of bool
| TPair of texpr * texpr
| TBinop of tbinop * texpr * texpr
| TUnop of tunop * texpr
| TIf of texpr * texpr * texpr
| TFun of string * typ * texpr
| TApp of texpr * texpr
| TLet of string * texpr * texpr
| TLetrec of string * texpr * texpr