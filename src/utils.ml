(**
   This module provides helpers to manipulate hexadecimal values
*)

open Char
open Opal

(** associates an integer value to an hexadecimal char *)
let hex_val c =
  match c with
  | 'a'..'f' -> (code c) - (code 'a') + 10
  | 'A'..'F' -> (code c) - (code 'A') + 10
  | '0'..'9' -> (code c) - (code '0')
  | _        ->
    Printf.eprintf "[hex_val] conversion error\n";
    exit 1

(** Parse an hexadecimal string and returns a list of hexadecimal chars *)
let parse_hex str =
  let hex_digit = spaces >> (upper <|> lower <|> digit) << spaces in
  let hex_str = many hex_digit in
  let input = LazyStream.of_string str in
  match hex_str input with
  | Some (x, Nil)  -> x
  | _ ->
    Printf.eprintf "[parse_hex] invalid hexadecimal string\n";
    exit 1

(** Convert a base16 number into a integer. The base16 number should be passed as a
    list of integers modulo 16. Most significant digits are left in the list *)
let int_of_base16 l =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 + x) xs
  in
  compute 0 l

(** Convert an integer in base16.
    Convert a base16 number into a integer. The base16 number is returned as a
    list of integers modulo 16. Most significant digits are left in the list *)
let base16_of_int i =
  let rec compute j =
    if j / 16 = 0 then [j]
    else (j mod 16)::(compute (j/16))
  in
  compute i |> List.rev

(** Convert a base16 number into a integer. The base16 number should be passed as a
    list of chars. Most significant digits are left in the list *)
let int_of_base16c l =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 + hex_val x) xs
  in
  compute 0 l

(** Return the integer value denoted by an hexadecimal string *)
let hex str =
  parse_hex str |> int_of_base16c

let (let*) = (>>=)

(** Parse an hexadecimal string as a sequence of bytes.
    Returns a list of integers modulo 256 *)
let parse_bytes str =
  let parse_byte =
    let* a = spaces >> (upper <|> lower <|> digit) in
    let* b = (upper <|> lower <|> digit) << spaces in
    return (16 * hex_val a + hex_val b)
  in
  let input = LazyStream.of_string str in
  match many (parse_byte) input with
  | Some (x, Nil)  -> x
  | _ ->
    Printf.eprintf "[parse_bytes] invalid sequence of bytes\n";
    exit 1

(** Convert an hexadecimal number giben as list of bytes
    (integer modulo 256) into an integer *)
let int_of_bytes bs =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 * 16 + x) xs
  in
  compute 0 bs

(** Convert an integer into an hexadecimal number represented as 
    list of bytes (integer modulo 256) *)
let bytes_of_int i =
  let rec compute j =
    if j / 256 = 0 then [j]
    else (j mod 256)::(compute (j / 256))
  in
  compute i |> List.rev

(** Convert an integer modulo 256 unto an hexadecimal string *)
let string_of_byte b =
  let str b =
    if b < 0 || b >= 16 then
      failwith "[string_of_byte] not a byte"
    else if b <= 9 then string_of_int b
    else String.make 1 (char_of_int (code 'a' + b - 10))
  in
  str (b / 16) ^ str (b mod 16)

(** Stringify a list of bytes *)
let string_of_bytes b =
  List.map string_of_byte b |> List.fold_left (fun acc c -> acc ^ " " ^ c) ""

(** Subtract an integer from a list of byte and returns a new list of bytes *)
let bytes_sub a l =
  int_of_bytes l - a |> bytes_of_int