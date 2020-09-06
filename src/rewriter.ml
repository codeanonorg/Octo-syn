open Asm

(** Rule based rewriter *)
type rewriter = Rewriter of string * (asm -> asm)