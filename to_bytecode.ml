open Ir

exception Error of string
let error_v msg v = 
  let msg =
    Format.fprintf Format.str_formatter "%s: %a@." msg Pp.pp_ir_value v;
    Buffer.contents Format.stdbuf
  in
  raise (Error msg)

let error msg =
  raise (Error msg)

let instruction_to_opcode instr =
  let aux = function
    | STOP  -> 0
    | DUPL  -> 1
    | SWAP  -> 2
    | ROT3  -> 3
    | IROT3 -> 4
    | FST   -> 5
    | SND   -> 6
    | SET_FST -> 7
    | SET_SND -> 8
    | CONS  -> 9
    | SPLIT -> 10 
    | ADD   -> 11
    | SUB   -> 12
    | MUL   -> 13
    | EQ    -> 14
    | LT    -> 15
    | CALL  -> 16
    | RETURN   -> 17
    | BRANCH _ -> 18
    | JUMP _   -> 19
    | LOAD _   -> 20 
    | PUSH NIL -> 21
    | PUSH _   -> 22
  in
  aux instr |> char_of_int

type code = { mutable memory : char Array.t; mutable pc : int }
let code = { memory = Array.make 16 '\x00'; pc = 0 }

let pointer_size = 4

let incr_pc () =
  code.pc <- code.pc + 1;
  let n = Array.length code.memory in 
  if code.pc >= n then begin 
    let a = Array.make (n*2) '\x00' in 
    Array.blit code.memory 0 a 0 n;
    code.memory <- a
  end  

let add instruction = 
  code.memory.(code.pc) <- instruction_to_opcode instruction;
  incr_pc ()

let add_number ?(update_pc=true) pc n =
  (* little endian *)
  let open Int32 in
  let shift n s =
    shift_right_logical n s 
    |> logand 0xffl
    |> to_int 
    |> char_of_int
  in
  code.memory.(pc) <- shift n 0; 
  if update_pc then incr_pc ();
  code.memory.(pc+1) <- shift n 8;
  if update_pc then incr_pc ();
  code.memory.(pc+2) <- shift n 16;
  if update_pc then incr_pc ();
  code.memory.(pc+3) <- shift n 24;
  if update_pc then incr_pc ()

let add_int ?(update_pc=true) pc i =   
  let open Int32 in
  let i = shift_left (of_int i) 1 |> logor 1l in
  add_number ~update_pc pc i

let add_bool ?(update_pc=true) pc b = 
  add_int ~update_pc pc (if b then 1 else 0)

let add_addr ?(update_pc=true) pc a =
  let open Int32 in
  let a = shift_left (of_int a) 1 in
  add_number ~update_pc pc a  

let ir_to_bytecode ir =
  let rec ir_to_bytecode = function
    | [STOP] -> 
        add STOP      
    | (LOAD (INT i) as instr) :: r
    | (PUSH (INT i) as instr) :: r ->  
        add instr;
        add_int code.pc i;
        ir_to_bytecode r
    | (LOAD (BOOL b) as instr) :: r
    | (PUSH (BOOL b) as instr) :: r ->
        add instr;
        add_bool code.pc b;
        ir_to_bytecode r      
    | (PUSH (ADDR l) as instr) :: r ->                        
        add (JUMP 0);
        let pc1 = code.pc in
        add_addr code.pc 0;
        let pc2 = code.pc in
        ir_to_bytecode l;
        add_addr ~update_pc:false pc1 code.pc;
        add instr;
        add_addr code.pc pc2; 
        ir_to_bytecode r        
    | (BRANCH (ADDR l1, ADDR l2) as instr) :: r ->         
        add instr;
        let pc1 = code.pc in 
        add_addr code.pc 0;
        let pc2 = code.pc in 
        add_addr code.pc 0;           
        add (JUMP 0);  
        let pc3 = code.pc in   
        add_addr code.pc 0;
        let pc_then = code.pc in
        ir_to_bytecode l1;
        let pc_else = code.pc in
        ir_to_bytecode l2;
        add_addr ~update_pc:false pc1 pc_then; 
        add_addr ~update_pc:false pc2 pc_else; 
        add_addr ~update_pc:false pc3 code.pc; 
        ir_to_bytecode r;
    | (BRANCH _) :: _ ->
        error "BRANCH must have two addresses as arguments"
    | STOP :: _ -> error "STOP must be the last instruction"
    | instr :: r -> 
        add instr;
        ir_to_bytecode r
    | []  -> ()
  in
  ir_to_bytecode ir;
  Array.sub code.memory 0 code.pc
  |> Array.to_list 
