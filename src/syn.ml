open Sem

let condition p = function
  | None -> false
  | Some x -> p x

(** A subset of x86 using only 'xor al' with operands in range [48..122] *)
let subset1 =
  List.init 74 (fun i -> XorAL (48 + i))

(** A subset of x86 using only 'xor al' with operands in range [48..122], 'push rax' & 'pop rax' *)
let subset2 =
  subset1 @ [PushRAX; PopRAX]

let syn k base test =
  let rec asm_alpha i =
    if i <= 1 then List.map (fun x -> [x]) base
    else begin
      let set1 = (asm_alpha (i - 1)) in
      let set2 = List.fold_left (fun acc prog ->
          (List.map (fun instr -> instr::prog) base) @ acc
        ) [] set1
      in set1 @ set2
    end
  in
  List.find (fun prog -> condition test (semp prog)) (asm_alpha k)

let print_instr = function
  | XorAL i ->
    Printf.printf "xor al, 0x%x" i
  | PushRAX ->
    Printf.printf "push rax"
  | PopRAX ->
    Printf.printf "pop rax"