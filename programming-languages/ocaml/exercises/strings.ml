(* 1: is_palindrome: string → bool
  that checks if the string is palindrome
  a string is palindrome when the represented sentence can be read the same way in either directions
  in spite of spaces, punctual and letter cases
 *)

let string_of_char c = String.make 1 c

 (* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 []
 
 (* Converts a list of chars to a string *)
let rec implode chars =
  match chars with
    [] -> ""
    | h::t ->  string_of_char h ^ (implode t)

let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h]

let keep_only (s1: string) (s2: string) =
  let s2' = explode s2 in
  let rec diff (s1': char list) (acc: char list) = match s1' with
    | [] -> acc
    | h :: t when List.mem h s2' -> diff t (h :: acc)
    | h :: t -> diff t acc
  in 
    implode (rev (diff (explode s1) []))

let only_letters s = keep_only (String.lowercase_ascii s) "abcdefghijklmonpqrstuvwxyz"

let is_palindrome (l: string) = explode (only_letters l) = rev (explode (only_letters l))

let () = match is_palindrome("AbcBa") with
  | true -> print_string("AbcBa is palindrome\n")
  | false -> print_string("AbcBa is not palindrome\n")
  let () = match is_palindrome("ciao") with
  | true -> print_string("ciao is palindrome\n")
  | false -> print_string("ciao is not palindrome\n")

(* 2: operator (-): string → string → string
  that subtracts the letters in a string from the letters in another string
  e.g., "Walter Cazzola"-"abcwxyz" will give "Wlter Col" note that the operator - is case sensitive
 *)

let ( - ) (s1: string) (s2: string) =
  let s2' = explode s2 in
  let rec diff (s1': char list) (acc: char list) = match s1' with
    | [] -> acc
    | h :: t when List.mem h s2' -> diff t acc
    | h :: t -> diff t (h :: acc)
  in 
    implode (rev (diff (explode s1) []))

let () = "Walter Cazzola" - "abcwxyz" |> print_string
let () = print_string "\n"

(*
3: anagram : string → string list → boolean
that given a dictionary of strings
checks if the input string is an anagram of one or more of the strings in the dictionary
*)

let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq

let anagram (s: string) (l: string list) =
  let rec is_anagram (w: string list) acc = match acc with
    | true -> true
    | false -> match w with
      | [] -> false
      | h :: t ->
        is_anagram t (sort h = sort s)
  in
    is_anagram l false

let () = match anagram "ciao" ["acio"; "oica"] with
  | true -> print_string("ciao has an anagram\n")
  | false -> print_string("ciao does not have an anagram\n")
let () = match anagram "arbok" ["kobra"; "broka"] with
  | true -> print_string("arbok has an anagram\n")
  | false -> print_string("arbok does not have an anagram\n")
let () = match anagram "ekans" ["s"] with
  | true -> print_string("ekans has an anagram\n")
  | false -> print_string("ekans does not have an anagram\n")