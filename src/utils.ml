open Char
open Opal

let hex_val c =
  match c with
  | 'a'..'f' -> (code c) - (code 'a') + 10
  | 'A'..'F' -> (code c) - (code 'A') + 10
  | '0'..'9' -> (code c) - (code '0')
  | _        ->
    Printf.eprintf "[hex_val] conversion error\n";
    exit 1

let parse_hex str =
  let hex_digit = spaces >> (upper <|> lower <|> digit) << spaces in
  let hex_str = many hex_digit in
  let input = LazyStream.of_string str in
  match hex_str input with
  | Some (x, Nil)  -> x
  | _ ->
    Printf.eprintf "[parse_hex] invalid hexadecimal string\n";
    exit 1

let hex str =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 + hex_val x) xs
  in
  parse_hex str
  |> compute 0

let (let*) = (>>=)

let parse_byte =
  let* a = spaces >> (upper <|> lower <|> digit) in
  let* b = (upper <|> lower <|> digit) << spaces in
  return (16 * hex_val a + hex_val b)

let parse_bytes str =
  let input = LazyStream.of_string str in
  match many (parse_byte) input with
  | Some (x, Nil)  -> x
  | _ ->
    Printf.eprintf "[parse_bytes] invalid hexadecimal string\n";
    exit 1