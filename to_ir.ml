open Ast_typ
open Ir 

let tunop_to_ir = function 
  | TFst -> [FST]
  | TSnd -> [SND]

let tbinop_to_ir = function 
  | TAdd -> [ADD]
  | TSub -> [SUB]
  | TMul -> [MUL]
  | TEq  -> [EQ]
  | TLt  -> [LT]

exception Error of string
let error msg = raise (Error msg)

let rec var_to_ir env v =
  match env with 
  | [] -> 
      error (Format.sprintf "unbound variable %s" v) 
  | v'::r ->
      if v = v' then [FST]
      else SND :: var_to_ir r v


let rec texpr_to_ir env e = 
  match e.tdescr with
  | TVar v ->
      var_to_ir env v   
  | TInt i ->
      [LOAD (INT i)]
  | TBool b ->
      [LOAD (BOOL b)]
  | TBinop (op, e1, e2) ->
      [DUPL]         
      @ texpr_to_ir env e2
      @ [SWAP]
      @ texpr_to_ir env e1
      @ tbinop_to_ir op
  | TUnop (op, e) ->
      texpr_to_ir env e
      @ tunop_to_ir op
  | TIf (c, e1, e2) ->
      [DUPL]         
      @ texpr_to_ir env c 
      @ [BRANCH (ADDR (texpr_to_ir env e1 @ [RETURN]), 
                 ADDR (texpr_to_ir env e2 @ [RETURN]))]
      @ [CALL]
  | TFun (x, _, e) ->        
      [
        PUSH (ADDR (texpr_to_ir (x :: env) e @ [RETURN])); 
        SWAP; 
        CONS
      ]
  | TApp (e1, e2) ->
      [DUPL]
      @ texpr_to_ir env e2
      @ [SWAP]
      @ texpr_to_ir env e1
      @ [
          SPLIT;
          IROT3; 
          CONS;
          SWAP; 
          CALL
        ]        
  | TPair (e1, e2) ->
      [DUPL]
      @ texpr_to_ir env e2
      @ [SWAP]
      @ texpr_to_ir env e1 
      @ [CONS]
  | TLet (x, e1, e2) ->
      [DUPL]
      @ texpr_to_ir env e1 
      @ [CONS]
      @ texpr_to_ir (x :: env) e2
  | TLetrec (f, e1, e2) ->      
      let env' = f :: env in
      [
        PUSH NIL;
        CONS
      ]      
      @ texpr_to_ir env' e1
      @ [
          DUPL;
          FST;
          SWAP;
          SET_FST
        ]
      @ texpr_to_ir env' e2
  
let tprogram_to_ir tprog =
  texpr_to_ir [] tprog @ [STOP]