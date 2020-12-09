
type asm =
    Xor of int

let sem al (Xor i) =
  assert (i <= 0xFF);
  (al lxor i)

let semp prog =
  List.fold_left (fun al instr -> sem al instr ) 0x00 prog

let syn k prog =
  let low = 48 in
  let hig = 122 in
  let instructions = List.init (hig - low) (fun i -> Xor (low + i)) in
  let rec asm_alpha i =
    if i <= 1 then List.map (fun x -> [x]) instructions
    else begin
      let set = (asm_alpha (i - 1)) in
      List.fold_left (fun acc prog ->
          (List.map (fun instr -> instr::prog) instructions) @ acc
        ) set set
    end
  in
  let sem_prog = semp prog in
  List.find (fun p -> semp p = sem_prog) (asm_alpha k)

let print_instr (Xor i) =
  Printf.printf "xor al, 0x%x" i

let _ =
  syn 3 [Xor (0x30)] |> List.iter (fun i -> print_instr i |> print_newline)