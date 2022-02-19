open Ast_loc
open Ast_typ

let (reset_id_counter, new_var) =
  let id = ref (-1) in
  (fun () -> id := 0),
  fun () -> incr id; Tvar { id = !id; def = None }

let rec head = function
  | Tvar { def = Some t; _ } -> head t
  | t -> t

let rec canon t = match head t with
  | Tvar _ | Tint | Tbool as t -> t
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)

module Vset = Set.Make(Int)

type schema = Forall of { vars : Vset.t ; typ : typ }

module Vmap = Map.Make(Int)

let instance = function
  | Forall { vars; typ } when Vset.is_empty vars -> 
      typ 
  | Forall { vars; typ } ->
      let new_vars =
        Vset.fold (fun v acc -> 
                    Vmap.add v (new_var ()) acc) 
                    vars Vmap.empty
      in
      let rec inst t = match head t with
        | Tint  -> Tint 
        | Tbool -> Tbool
        | Tarrow (t1, t2)   -> Tarrow (inst t1, inst t2) 
        | Tproduct (t1, t2) -> Tproduct (inst t1, inst t2)
        | Tvar { id; _ } as v ->
          try 
            Vmap.find id new_vars
          with Not_found -> v        
      in
      inst typ

let rec vars_of_type t = match head t with
  | Tint | Tbool -> Vset.empty
  | Tarrow (t1, t2)    
  | Tproduct (t1, t2) -> Vset.union (vars_of_type t1) (vars_of_type t2)
  | Tvar { id; _ } -> Vset.singleton id 


module Env = Map.Make(String)

let vars_of_env env =
    Env.fold (fun _ (Forall { vars; typ }) acc -> 
        Vset.union acc (Vset.diff (vars_of_type typ) vars)
    ) env Vset.empty

let generalize env typ =
  let vars =
    Vset.diff (vars_of_type typ) (vars_of_env env)
  in  
  Forall { vars; typ }

exception NotUnifiable of typ * typ

let not_unifiable t1 t2 = raise (NotUnifiable (canon t1, canon t2))

let rec occurs v t = match head t with
  | Tvar w -> v.id = w.id
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> occurs v t1 || occurs v t2
  | Tint | Tbool -> false

let rec unify t1 t2 =
  match head t1, head t2 with 
    | Tint, Tint -> ()
    | Tbool, Tbool -> ()
    | Tvar v1, Tvar v2 when v1.id = v2.id -> ()
    | Tvar v1 as t1, t2 ->
        if occurs v1 t2 then not_unifiable t1 t2;
        assert (v1.def = None);
        v1.def <- Some t2
    | t1, (Tvar _ as t2) -> 
        unify t2 t1 
    | Tarrow (t1, t2), Tarrow (t1', t2')
    | Tproduct (t1, t2), Tproduct (t1', t2') -> 
        unify t1 t1';
        unify t2 t2' 
    | t1, t2 ->
        not_unifiable t1 t2


exception Error of string
let error loc msg =
  let pos = Location.startpos loc in
  let l = pos.Lexing.pos_lnum in 
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  raise @@ 
    Error (Format.sprintf "@[<hov 2>%s at line %d,@ characted %d@]@." msg l c)

let w e =
  let mk tdescr typ = { tdescr; typ } in
  let rec w env e = 
    try 
      match e.descr with
      | Var x -> begin
          try
            mk (TVar x) (Env.find x env |> instance)
          with Not_found -> 
            Format.sprintf "variable %s not found" x |> error e.loc 
      end
      | Int i ->
          mk (TInt i) Tint
      | Bool b ->
          mk (TBool b) Tbool
      | Binop (op, e1, e2) ->
          let e1' = w env e1 in          
          let e2' = w env e2 in
          let op', top1, top2, tret =
            match op with 
            | Add -> TAdd, Tint, Tint, Tint
            | Sub -> TSub, Tint, Tint, Tint
            | Mul -> TMul, Tint, Tint, Tint
            | Eq  -> TEq,  e1'.typ, e1'.typ, Tbool
            | Lt  -> TLt,  e1'.typ, e1'.typ, Tbool
          in
          unify e1'.typ top1;
          unify e2'.typ top2;
          mk (TBinop (op', e1', e2')) tret             
      | Unop (op, e) ->
          let e' = w env e in 
          let v1 = new_var () in 
          let v2 = new_var () in          
          unify e'.typ (Tproduct (v1, v2));
          let op', tret =
            match op with
            | Fst -> TFst, v1 
            | Snd -> TSnd, v2
          in
          mk (TUnop (op', e')) tret
      | Pair (e1, e2) -> 
          let e1' = w env e1 in
          let e2' = w env e2 in
          mk (TPair (e1', e2')) (Tproduct (e1'.typ, e2'.typ))
      | If (c, e1, e2) -> 
          let c' = w env c in
          unify c'.typ Tbool;
          let e1' = w env e1 in
          let e2' = w env e2 in
          unify e1'.typ e2'.typ; 
          mk (TIf (c', e1', e2')) e1'.typ
      | Fun (x, e) ->
          let v = new_var () in
          let env' = Env.add x (Forall { vars = Vset.empty; typ = v }) env in
          let e' = w env' e in      
          mk (TFun (x, v, e')) (Tarrow (v, e'.typ))
      | App (e1, e2) ->
          let e1' = w env e1 in
          let e2' = w env e2 in
          let v = new_var () in
          unify e1'.typ (Tarrow (e2'.typ, v));
          mk (TApp (e1', e2')) v
      | Let (x, e1, e2) ->
          let e1' = w env e1 in
          let env' = Env.add x (generalize env e1'.typ) env in
          let e2' = w env' e2 in 
          mk (TLet (x, e1', e2')) e2'.typ
      | Letrec (f, e1, e2) ->
          let v1 = new_var () in
          let v2 = new_var () in
          let tf = Tarrow (v1, v2) in
          let env' = Env.add f (Forall { vars = Vset.empty; typ = tf }) env in
          let e1' = w env' e1 in          
          unify e1'.typ tf;
          let env' = Env.add f (generalize env tf) env in
          let e2' = w env' e2 in          
          mk (TLetrec (f, e1', e2')) e2'.typ
    with NotUnifiable (t1, t2) ->
      let msg =
        Format.fprintf Format.str_formatter "@[<hov 2>%a and@ %a are not unifiable@]@."
          Pp.pp_typ t1 Pp.pp_typ t2;
        Buffer.contents Format.stdbuf
      in
      error e.loc msg
  in
  w Env.empty e

let typeof e = 
  reset_id_counter ();
  let { tdescr; typ } = w e in
  { tdescr; typ = canon typ }

