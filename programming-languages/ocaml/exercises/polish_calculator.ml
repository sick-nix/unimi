(* 
Write a PolishCalculator module that implements a stack-based calculator
  that adopts polish notation for the expressions to be evaluated.
Polish Notation is a prefix notation wherein every operator follows all of its operands;
  this notation has the big advantage of being unambiguous and permits to avoid the use of parenthesis.
  E.g., (3+4)*5 is equal to 3 4 + 5 *.
The module should include an:
  Expr datatype representing an expression
  a function expr_of+string: string → Expr which build the expression in the corresponding infix notation
    out of the string in polish notation;
  a function eval: Expr → int which will evaluate the expression and returns such evaluation
The recognized operators should be +, - (both unary and binary), *, /, ** over integers.
  At least a space ends each operands and operators.
The evaluation/translation can be realized by pushing the recognized elements on a stack.
  Define the module independently of the Stack implementation and try to use functors to adapt it. 
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
    | [] -> ""
    | h::t ->  string_of_char h ^ (implode t)

let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h]

module PolishCalculator = struct
  type toperator = Addition | Subtraction | Multiplication | Division | Exponentiation | Nop
  type token =
    | Number of float
    | Expr of toperator * token list
  type expr = toperator * token list
  let empty = Expr(Nop, [])
  let string_of_operator o = match o with
  | Addition -> "addition"
  | Subtraction -> "subtraction"
  | Multiplication -> "multiplication"
  | Division -> "division"
  | Exponentiation -> "exponentiation"
  | Nop -> "NOP"
  let operator_of_string s = match s with
  | "+" -> Addition
  | "-" -> Subtraction
  | "*" -> Multiplication
  | "/" -> Division
  | "**" -> Exponentiation
  | _ -> Nop
  let print_expr (m : token) =
    let rec pr (l: token list) = match l with
      | [] -> ()
      | h :: t -> match h with
        | Number v -> Format.printf "%f\n" v; pr t
        | Expr (o, ops) ->
          Format.printf "%s ->\n" (string_of_operator o); pr ops; pr t
        in pr [m]
   let expr_of_string (s: string) =
    let s' = String.split_on_char ' ' s in
    let rec build_expr (l: string list) (operands: token list) = match l with
    | [] -> operands
    | h :: t -> match h with
      | ("+"|"-"|"/"|"*"|"**") -> build_expr t [Expr(operator_of_string(h), List.rev operands)]
      (* | "-" -> build_expr t [Expr(Subtraction, operands)]
      | "+" -> build_expr t [Expr(Addition, operands)]
      | "/" -> build_expr t [Expr(Division, operands)]
      | "*" -> build_expr t [Expr(Multiplication, operands)]
      | "**" -> build_expr t [Expr(Exponentiation, operands)] *)
      | x -> match float_of_string_opt(x) with
        | Some v -> build_expr t (Number(v) :: operands)
        | None -> build_expr t operands in
    match List.nth_opt (build_expr s' []) 0 with
    | Some v -> v
    | None -> empty
  let eval (t: token) =
    let rec eval_token (t: token) = match t with
    | Number n -> n
    | Expr (o, ops) -> match ops with
      | h :: [] -> (match o with
        | Subtraction -> (0. -. eval_token h)
        | _ -> 0.)
      | h1 :: h2 :: [] -> (match o with
        | Addition -> eval_token h1 +. eval_token h2
        | Subtraction -> eval_token h1 -. eval_token h2
        | Multiplication -> eval_token h1 *. eval_token h2
        | Division -> eval_token h1 /. eval_token h2
        | Exponentiation -> Float.pow (eval_token h1) (eval_token h2)
        | Nop -> 0.
      )
      | _ -> 0.
    in
    eval_token t
end

(* let () = PolishCalculator.expr_of_string "3 4 + 5 *" |> PolishCalculator.print_expr *)
(* ; print_string "\n" ; *)
let () = PolishCalculator.expr_of_string "3 4 + 5 *" |> PolishCalculator.eval |> print_float
; print_string "\n"

let () = PolishCalculator.expr_of_string "3 - 5 +" |> PolishCalculator.eval |> print_float