open Asm

type memory = Mem of (int -> int)

let semA (c : asm) =
  let rec eval m r = function
    | ADD (Reg x, Reg y, Reg z) -> m.(z) = m.(x) + m.(y)
    | SUB (Reg x, Reg y, Reg z) -> m.(z) = m.(x) - m.(y)
    | CLL (Adr i)
    | RET
    | JMP of adr
    | PSH of reg
    | POP of reg
    | LDR of adr * reg
    | STR of reg * adr
  in
  eval (Array.make 512 0) (Array.make 16 0) c