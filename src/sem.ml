
type asm =
  | XorAL of int
  | PushRAX
  | PopRAX

type state = {
  regA : int;
  stack : int list;
}

let init_state = Some {regA = 0; stack = []}

let sem instr state =
  match instr with
  | XorAL i -> Some {state with regA = state.regA lxor (i land 0xff)}
  | PushRAX -> Some {state with stack = state.regA::state.stack}
  | PopRAX ->
    match state.stack with
    | [] -> None
    | x::xs -> Some {stack = xs; regA = x}

let (>>=) = Option.bind

let semp prog =
  List.fold_left (fun al instr -> al >>= sem instr) init_state prog