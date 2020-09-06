type reg = Reg of int
type adr = Adr of int

type asm =
  | ADD of reg * reg
  | SUB of reg * reg
  | CLL of adr
  | RET
  | JMP of adr
  | PSH of reg
  | POP of reg
  | LDR of adr * reg
  | STR of reg * adr

type memo =
  | M_ADD of reg * reg
  | M_SUB of reg * reg
  | M_CLL of adr
  | M_RET
  | M_JMP of string
  | M_LBL of string
  | M_PSH of reg
  | M_POP of reg
  | M_LDR of adr * reg
  | M_STR of reg * adr

module type COMPILER_SIG = sig
  type opcode
  type compiler = asm -> opcode list
end