open Ir

exception Execution_error

let exec ir =
  let rec exec = function 
    | ([STOP], [v]) -> 
        v
    | ((LOAD v)::code, _::stack) -> 
        exec (code, v::stack)
    | ((PUSH v)::code, stack) -> 
        exec (code, v::stack)
    | (DUPL::code, v::stack) -> 
        exec (code, v::v::stack)
    | (SWAP::code, v::v'::stack) -> 
        exec (code, v'::v::stack)
    | (ROT3::code, v1::v2::v3::stack) -> 
        exec (code, v2::v3::v1::stack)
    | (IROT3::code, v1::v2::v3::stack) -> 
        exec (code, v3::v1::v2::stack)
    | (FST::code, (PAIR (v1, _))::stack) -> 
        exec (code, !v1::stack)
    | (SND::code, (PAIR (_, v2))::stack) ->  
        exec (code, !v2::stack)
    | (SET_FST::code, v::(PAIR (v1, _) as p)::stack) -> 
        v1 := v;
        exec (code, p::stack)
    | (SET_SND::code, v::(PAIR (_, v2) as p)::stack) ->  
        v2 := v;
        exec (code, p::stack)
    | (CONS::code, v1::v2::stack) -> 
        exec (code, (PAIR (ref v1, ref v2))::stack)
    | (SPLIT::code, (PAIR (v1, v2))::stack) ->  
        exec (code, !v1::!v2::stack)
    | (ADD::code, (INT v1)::(INT v2)::stack) -> 
        exec (code, (INT (v1 + v2))::stack)
    | (SUB::code, (INT v1)::(INT v2)::stack) -> 
        exec (code, (INT (v1 - v2))::stack)
    | (MUL::code, (INT v1)::(INT v2)::stack) -> 
        exec (code, (INT (v1 * v2))::stack)
    | (EQ::code, v1::v2::stack) -> 
        exec (code, (BOOL (v1 = v2))::stack)
    | (LT::code, v1::v2::stack) -> 
        exec (code, (BOOL (v1 < v2))::stack)
    | (CALL::code, (ADDR code')::v::stack) -> 
        exec (code', v::(ADDR code)::stack)
    | (RETURN::_, v::(ADDR code')::stack) -> 
        exec (code', v::stack)
    | (BRANCH(addr1, addr2)::code, (BOOL c)::stack) -> 
        if c then 
          exec (code, addr1::stack)
        else 
          exec (code, addr2::stack)
    | _  -> 
        raise Execution_error
  in
  exec (ir, [NIL])