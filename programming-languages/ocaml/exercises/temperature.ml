type tscale = Celsius | Fahrenheit | Kelvin | Rankine | Delisle | Newton | Reaumur | Romer
type temperature = {scale: tscale; value: float}

let any2c t = match t.scale with
  | Celsius -> t
  | Fahrenheit -> {value = (t.value -. 32.) *. 5. /. 9.; scale = Celsius}
  | Kelvin -> {value = t.value -. 273.15; scale = Celsius}
  | Rankine -> {value = (t.value -. 491.67) *. 5. /. 9.; scale = Celsius}
  | Delisle -> {value = 100. -. (t.value *. 2. /. 3.); scale = Celsius}
  | Newton -> {value = t.value *. 100. /. 33.; scale = Celsius}
  | Reaumur -> {value = t.value *. 5. /. 4.; scale = Celsius}
  | Romer -> {value = (t.value -. 7.5) *. 40. /. 21.; scale = Celsius}

let c2any u t = match u with
  | Celsius -> t
  | Fahrenheit -> {value = (t.value *. 9. /. 5.) +. 32.; scale = u}
  | Kelvin -> {value = t.value +. 273.15; scale = u}
  | Rankine -> {value = (t.value +. 273.15) *. 9. /. 5.; scale = u}
  | Delisle -> {value = (100. -. t.value) *. 3. /. 2.; scale = u}
  | Newton -> {value = t.value *. 33. /. 100.; scale = u}
  | Reaumur -> {value = t.value *. 4. /. 5.; scale = u}
  | Romer -> {value = (t.value *. 21. /. 40.) +. 7.5; scale = u}

let temp_degree_symbol t = match t.scale with
  | Celsius -> "°C"
  | Fahrenheit -> "°F"
  | Kelvin -> "°K"
  | Rankine -> "°R"
  | Delisle -> "°De"
  | Newton -> "°N"
  | Reaumur -> "°Re"
  | Romer -> "°Ro"

let print_temp t = Printf.printf "%6.1f%s" t.value (temp_degree_symbol t)

let rec print_temp_list = function
  | [] -> ()
  | h :: t -> print_temp h; print_string ";\t"; print_temp_list t

let scales = [Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer]

(* 2: Write a function that given a temperature in a specified scale
  returns a list of all the corresponding temperatures in the other scales,
  note that the scale must be specified *)

  let rec get_convertions_acc l temp = match l with
  | [] -> []
  | h :: t ->
    if h = temp.scale
      then (get_convertions_acc t temp)
      else (any2c temp |> c2any h) :: (get_convertions_acc t temp)

let get_convertions = get_convertions_acc scales

let temp = {scale = Celsius; value = 23.}

let () = print_temp_list (get_convertions temp)
let () = print_string "\n\n"

(* 1: Write a function that given a pure number
  returns a conversion table for it among any of the 8 scales *)

let rec print_all_convertions_acc l v = match l with
  | [] -> ()
  | h :: t ->
    print_temp {scale = h; value = v};
    print_string "\t->\t";
    print_temp_list (get_convertions {scale = h; value = v});
    print_string "\n";
    print_all_convertions_acc t v

let print_all_convertions = print_all_convertions_acc scales
let () = print_all_convertions 23.