Require Import Arith ZArith Coq.Strings.Byte List.
Import List.ListNotations.

Definition cond : Type := option comparison.

Inductive reg := Reg : nat -> reg.

Inductive adr := Adr : nat -> adr.

Inductive asm : Type :=
  | Cpy : reg -> reg -> asm
  | Mov : Z -> reg -> asm
  | Add : reg -> reg -> reg -> asm
  | Sub : reg -> reg -> reg -> asm
  | Cmp : reg -> reg -> asm
  | Ldr : adr -> reg -> asm
  | Psh : reg -> asm
  | Pop : reg -> asm
  | Str : reg -> adr -> asm
  | Jmp : cond -> adr -> asm.

Definition store := adr -> Z.

Definition stack := list Z.

Definition registers := reg -> Z.

Definition program := list asm.

Definition config := (store * registers * stack * nat * cond)%type.

Definition update_reg (r : reg) (v : Z) f :=
  let '(Reg n) := r in
  fun r' => (
    let '(Reg n') := r' in
    if n =? n then v else f r'
  ).

Definition update_mem (a : adr) (v : Z) f :=
  let '(Adr n) := a in
  fun a' => (
    let '(Adr n') := a' in
    if n' =? n then v else f a'
  ).

Definition instr_at (P : program) (pc : nat) := nth_error P pc.

Definition test (v1 v2 : Z) :=
  if (v1 =? v2)%Z then Some Eq else
  if (v1 <? v2)%Z then Some Lt else
  if (v1 =? v2)%Z then Some Gt else None.

Definition get_a (a : adr) :=
  let '(Adr n) := a in n.

Inductive sem_asm (P : program) : config -> config -> Prop :=
  | sem_add : forall r1 r2 r3 mem rgs stk pc cd,
    instr_at P pc = Some (Add r1 r2 r3) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, update_reg r3 (rgs r1 + rgs r2)%Z rgs, stk, pc + 1, cd)
    
  | sem_sub : forall r1 r2 r3 mem rgs stk pc cd,
    instr_at P pc = Some (Sub r1 r2 r3) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, update_reg r3 (rgs r1 - rgs r2)%Z rgs, stk, pc + 1, cd)
    
  | sem_cpy : forall r1 r2 mem rgs stk pc cd,
    instr_at P pc = Some (Cpy r1 r2) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, update_reg r2 (rgs r1) rgs, stk, pc + 1, cd)
    
  | sem_mov : forall c r mem rgs stk pc cd,
    instr_at P pc = Some (Mov c r) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, update_reg r c rgs, stk, pc + 1, cd)
  
  | sem_cmp : forall r1 r2 mem rgs stk pc cd,
    instr_at P pc = Some (Cmp r1 r2) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, rgs, stk, pc + 1, test (rgs r1) (rgs r2))

  | sem_ldr : forall a r mem rgs stk pc cd,
    instr_at P pc = Some (Ldr a r) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, update_reg r (mem a) rgs, stk, pc + 1, cd)
    
  | sem_str : forall a r mem rgs stk pc cd,
    instr_at P pc = Some (Str r a) ->
    sem_asm P (mem, rgs, stk, pc, cd) (update_mem a (rgs r) mem, rgs, stk, pc + 1, cd)
    
  | sem_psh : forall r mem rgs stk pc cd,
    instr_at P pc = Some (Psh r) ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, rgs, (rgs r)::stk, pc + 1, cd)
  
  | sem_pop : forall q r mem rgs stk pc cd,
    instr_at P pc = Some (Pop r) ->
    sem_asm P (mem, rgs, q::stk, pc, cd) (mem, update_reg r q rgs, stk, pc + 1, cd)
  
  | sem_jmp : forall a t mem rgs stk pc cd,
    instr_at P pc = Some (Jmp t a) ->
    cd = t ->
    sem_asm P (mem, rgs, stk, pc, cd) (mem, rgs, stk, get_a a, cd).


