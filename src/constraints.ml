open Asm

(** Syntactical constraint *)
type syn_constraint = Syn_Constraint of (asm list -> bool)

(** Solver *)
type solver = Solver of (syn_constraint list -> asm list -> asm list)