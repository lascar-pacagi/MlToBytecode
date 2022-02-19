type instruction =
| STOP 
| LOAD of value 
| PUSH of value 
| DUPL 
| SWAP 
| ROT3
| IROT3
| FST
| SND
| SET_FST
| SET_SND
| CONS 
| SPLIT 
| ADD
| SUB
| MUL 
| EQ 
| LT 
| CALL 
| RETURN 
| BRANCH of value * value 
| JUMP of int

and value =
| INT of int 
| BOOL of bool 
| NIL 
| ADDR of instruction list
| PAIR of value ref * value ref (* only used for ir simulator *)
