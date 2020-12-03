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

let int_of_base16 l =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 + x) xs
  in
  compute 0 l

let base16_of_int i =
  let rec compute j =
    if j / 16 = 0 then [j]
    else (j mod 16)::(compute (j/16))
  in
  compute i |> List.rev

let int_of_base16c l =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 + hex_val x) xs
  in
  compute 0 l

let hex str =
  parse_hex str |> int_of_base16c

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

let int_of_bytes bs =
  let rec compute acc = function
    | []    -> acc
    | x::xs -> compute (acc * 16 * 16 + x) xs
  in
  compute 0 bs

let bytes_of_int i =
  let q = 16 * 16 in
  let rec compute j =
    if j / q = 0 then [j]
    else (j mod q)::(compute (j/q))
  in
  compute i |> List.rev

let string_of_byte b =
  let str b =
    if b < 0 || b >= 16 then
      failwith "[string_of_byte] not a byte"
    else if b <= 9 then string_of_int b
    else String.make 1 (char_of_int (code 'a' + b - 10))
  in
  str (b / 16) ^ str (b mod 16)

let string_of_bytes b =
  List.map string_of_byte b |> List.fold_left (fun acc c -> acc ^ " " ^ c) ""

let bytes_sub a l =
  int_of_bytes l - a |> bytes_of_int