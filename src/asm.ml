open Keystone
open Keystone.Types

let asm_size res = res.encoding_size

let asm_count res = res.stat_count

let asm_bytes res = res.encoding

let print_bytes bytes = asm_array_to_string bytes 
                        |> print_endline

let reverted arr =
  let n = Array.length arr in
  Array.init n (fun i -> arr.(n - i - 1))

let asm str =
  let engine =
    match ks_open KS_ARCH_X86 KS_MODE_64 with
    | Ok e    -> e
    | Error _ -> assert false
  in
  match ks_asm engine str 0 with
  | Ok res  -> ignore(ks_close engine); res
  | Error s ->
    ignore (ks_close engine);
    Printf.eprintf "[asm] failed with: %s\n" s;
    exit 1