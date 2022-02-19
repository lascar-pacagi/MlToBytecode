open Ast_loc 
open Ast_typ
open Format
open Ir 

let rec pp_expr fmt { descr; _ } =
  let pp_binop fmt = function 
    | Add -> fprintf fmt "+"
    | Sub -> fprintf fmt "-"
    | Mul -> fprintf fmt "*"
    | Eq  -> fprintf fmt "="
    | Lt  -> fprintf fmt "<"
  in
  let pp_unop fmt = function 
    | Fst -> fprintf fmt "fst"
    | Snd -> fprintf fmt "snd"
  in 
  let pp_descr fmt = function
    | Var v ->
        fprintf fmt "%s" v
    | Int i ->
        fprintf fmt "%i" i
    | Bool b ->
        fprintf fmt "%B" b
    | Binop (op, e1, e2) ->
        fprintf fmt "(%a@ %a@ %a)" 
          pp_expr e1 
          pp_binop op 
          pp_expr e2 
    | Unop (op, e) ->
        fprintf fmt "(%a@ %a@)" pp_unop op pp_expr e
    | If (c, e1, e2) ->
        fprintf fmt "if %a then@ %a@ else %a" 
          pp_expr c 
          pp_expr e1
          pp_expr e2    
    | Fun (x, e) ->
        fprintf fmt "(fun %s ->@ %a)" x pp_expr e
    | App (e1, e2) ->
        fprintf fmt "(%a@ %a)" pp_expr e1 pp_expr e2
    | Pair (e1, e2) ->
        fprintf fmt "(%a,@,%a)" pp_expr e1 pp_expr e2
    | Let (x, e1, e2) ->
        fprintf fmt "let %s =@ %a in@ %a" x pp_expr e1 pp_expr e2
    | Letrec (f, e1, e2) ->
        fprintf fmt "let rec %s =@ %a in@ %a" f pp_expr e1 pp_expr e2
  in
  fprintf fmt "@[<hov 2>%a@]" pp_descr descr

let pp_program fmt prog =
  pp_expr fmt prog

let rec pp_typ fmt = function
  | Tproduct (t1, t2) -> fprintf fmt "@[<hov 2>%a *@ %a@]" pp_atom t1 pp_atom t2
  | Tarrow (t1, t2) -> fprintf fmt "@[<hov 2>%a ->@ %a@]" pp_atom t1 pp_typ t2
  | (Tint | Tbool | Tvar _) as t -> pp_atom fmt t
and pp_atom fmt = function
  | Tint  -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tvar v -> pp_tvar fmt v
  | Tarrow _ | Tproduct _ as t -> fprintf fmt "@[<hov 2>(%a)@]" pp_typ t
and pp_tvar fmt = function
  | { def = None; id } -> fprintf fmt "'%d" id
  | { def = Some t; _ } -> fprintf fmt "(%a)" pp_typ t

let rec pp_texpr fmt { tdescr; typ } =
  let pp_tbinop fmt = function 
    | TAdd -> fprintf fmt "+"
    | TSub -> fprintf fmt "-"
    | TMul -> fprintf fmt "*"
    | TEq  -> fprintf fmt "="
    | TLt  -> fprintf fmt "<"
  in
  let pp_tunop fmt = function 
    | TFst -> fprintf fmt "fst"
    | TSnd -> fprintf fmt "snd"
  in 
  let pp_tdescr fmt = function 
    | TVar v ->
        fprintf fmt "%s" v
    | TInt i ->
        fprintf fmt "%i" i
    | TBool b ->
        fprintf fmt "%B" b
    | TBinop (op, e1, e2) ->
        fprintf fmt "(%a@ %a@ %a)" 
          pp_texpr e1 
          pp_tbinop op 
          pp_texpr e2 
    | TUnop (op, e) ->
        fprintf fmt "(%a@ %a@)" pp_tunop op pp_texpr e
    | TIf (c, e1, e2) ->
        fprintf fmt "if %a then@ %a@ else %a" 
          pp_texpr c 
          pp_texpr e1
          pp_texpr e2  
    | TFun (x, typ, e) ->
        fprintf fmt "(fun (%s : %a) ->@ %a)" x pp_typ typ pp_texpr e
    | TApp (e1, e2) ->
        fprintf fmt "(%a@ %a)" pp_texpr e1 pp_texpr e2
    | TPair (e1, e2) ->
        fprintf fmt "(%a,@,%a)" pp_texpr e1 pp_texpr e2
    | TLet (x, e1, e2) ->
        fprintf fmt "let %s =@ %a in@ %a" x pp_texpr e1 pp_texpr e2
    | TLetrec (f, e1, e2) ->
        fprintf fmt "let rec %s =@ %a in@ %a" f pp_texpr e1 pp_texpr e2
  in
  fprintf fmt "@[<hov 2>(%a :@ %a)@]" pp_tdescr tdescr pp_typ typ 

let pp_tprogram fmt prog =
  pp_texpr fmt prog
  
let rec pp_ir_instruction fmt = function 
  | STOP -> 
      fprintf fmt "STOP"
  | LOAD v ->
      fprintf fmt "LOAD(%a)" pp_ir_value v 
  | PUSH v ->
      fprintf fmt "PUSH(%a)" pp_ir_value v
  | DUPL ->
      fprintf fmt "DUPL"
  | SWAP ->
      fprintf fmt "SWAP"
  | ROT3 ->
      fprintf fmt "ROT3"
  | IROT3 ->
      fprintf fmt "IROT3"
  | FST ->
      fprintf fmt "FST"
  | SND ->
      fprintf fmt "SND"
  | SET_FST ->
      fprintf fmt "SET_FST"
  | SET_SND ->
      fprintf fmt "SET_SND"
  | CONS ->
      fprintf fmt "CONS"
  | SPLIT ->
      fprintf fmt "SPLIT"
  | ADD ->
      fprintf fmt "ADD"
  | SUB ->
      fprintf fmt "SUB"
  | MUL ->
      fprintf fmt "MUL"
  | EQ ->
      fprintf fmt "EQ"
  | LT ->
      fprintf fmt "LT"
  | CALL ->
      fprintf fmt "CALL"
  | RETURN ->
      fprintf fmt "RETURN"
  | BRANCH (l1, l2) ->
      fprintf fmt "BRANCH(@[<hov>%a,@ %a@])" 
        pp_ir_value l1
        pp_ir_value l2
  | JUMP i ->
    fprintf fmt "JUMP(%d)" i      
  
and pp_ir_value fmt = function 
  | INT i  -> 
      fprintf fmt "%i" i
  | BOOL b -> 
      fprintf fmt "%B" b
  | NIL    -> 
      fprintf fmt "nil"
  | ADDR l -> 
      fprintf fmt "ADDR(@[<v>%a@])"
        (pp_print_list ~pp_sep:pp_print_cut pp_ir_instruction) l
  | PAIR (v1, v2) ->
      fprintf fmt "PAIR(@[<hov>%a,@ %a@])"
        pp_ir_value !v1
        pp_ir_value !v2   
        
let pp_bytecode fmt bc =
  fprintf fmt "%02x" (int_of_char bc)