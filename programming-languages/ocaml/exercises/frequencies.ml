(*
Let's write a function (or a pool of functions) that given a quite large text (over than 2000 words)
  counts how frequent each word occurs in the text.
The text is read from a file (look at the pervasive module in the manual) and it is a real text with punctuation
  (i.e., commas, semicolons, ...) that should be counted.
Note that words with different case should be considered the same.
*)

module StringMap = Map.Make(String)

let string_of_char c = String.make 1 c

(* Converts a string to a list of char s *)
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

let pp_map ppf (m : int StringMap.t) =
  StringMap.iter (fun k v -> Format.fprintf ppf "%s -> %d@\n" k v) m


let read_file = 
  let map: int StringMap.t ref = ref StringMap.empty in
  let rec analyze_words (l: string list) (m: int StringMap.t) = match l with
    | [] -> m
    | h :: t ->
      let w = only_letters h in
      match StringMap.mem w m with
        | true -> analyze_words t (StringMap.update w (Option.map (fun v -> v+1)) m)
        | false -> analyze_words t (StringMap.add w 1 m)
    in
  let chan = open_in "file.txt" in
  try
    while true do
      let words = input_line chan |> String.split_on_char ' ' in
      map := analyze_words words !map
    done; !map
  with End_of_file ->
    close_in chan;
    !map

let () = read_file |> Format.printf "%a" pp_map