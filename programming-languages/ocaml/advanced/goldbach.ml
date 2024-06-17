(* 
Goldbach's conjecture is one of the oldest unsolved problems in number theory and in all of mathematics. It states:
Every even integer greater than 2 is a Goldbach number, i.e., a number that can be expressed as the sum of two odd primes.
Expressing a given even number as a sum of two primes is called a Goldbach partition of the number. For example,
4 = 2 +  2           6 = 3 +  3           8 = 3 +  5
10 = 7 +  3          12 = 5 +  7          14 = 3 + 11
16 = 5 + 11          18 = 7 + 11          20 = 7 + 13

Write the following functions:
  goldbach(n) that returns a Goldbach partition for n
  goldbach_list(n,m) that returns a list of Goldbach partitions for the even numbers in the range (n,m).
*)

module IntMap = Map.Make(Int)

let rec print_list = function
  | [] -> ()
  | h :: t ->
    Printf.printf "%d\t" h;
    print_list t

let print_map m =
  IntMap.iter (fun key value -> Printf.printf "%d -> " key; print_list value; print_string "\n") m;;

let factors n =
  let rec factors_acc n' i acc = match n' with
  | 1 -> 1 :: acc
  | x when n' < n / 2 -> 1 :: acc
  | x -> match n' mod i with
    | 0 -> factors_acc (Int.div n' i) i (i :: acc)
    | _ -> factors_acc n' (i+1) acc
  in
  factors_acc n 2 []

let is_prime n = factors n = [1; n]

let goldbach n =
  if is_prime n then
    []
  else
    let rec goldbach_acc n' = match n' with
    | 0 -> []
    | _ -> match is_prime n' && is_prime (n-n') with
      | true -> [n'; n-n']
      | false -> goldbach_acc (n'-1)
    in
    List.sort compare (goldbach_acc (n-1))

let goldbach_list n m =
  let rec goldbach_inner n' map' = match m - n' with
  | -1 -> map'
  | x when x mod 2 != 0 -> goldbach_inner (n'+1) map'
  | _ -> goldbach_inner (n'+1) (IntMap.add n' (goldbach n') map')
  in
    goldbach_inner n IntMap.empty

let test_goldbach =
  assert(goldbach 4 = [2;2]);
  assert(goldbach 6 = [3;3]);
  assert(goldbach 8 = [3;5]);
  assert(goldbach 10 = [3;7]);
  assert(goldbach 12 = [5;7]);
  assert(goldbach 14 = [3;11]);
  assert(goldbach 16 = [3;13]);
  assert(goldbach 18 = [5;13]);
  assert(goldbach 20 = [3;17])

let test_goldbach_list =
  let res = goldbach_list 4 20 in
  print_map res

let () = test_goldbach
let () = test_goldbach_list