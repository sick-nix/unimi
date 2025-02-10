(*
Create a graph module with a functor with functions like:
- add_node, add_edge, get_edges(node), get_nodes(edge)
*)

module type GenericType = sig
  type t
end

module Graph = functor(Elt: GenericType) -> struct
  type t = Elt.t
  type tnode = t
  type tedge = (tnode * tnode)
  type tnodes = tnode list
  type tedges = tedge list
  type tgraph = tnodes * tedges
  
  let empty: tgraph = ([], [])

  let add_node (n: tnode) (g: tgraph) = match g with
  | (nd, ed) -> ((nd @ [n]), ed)

  let are_connected (n1: tnode) (n2: tnode) (g: tgraph) = 
    let rec has_edge (edges: tedges) (acc: bool) = if acc then true else match edges with
    | [] -> false
    | (n', n'') :: t -> has_edge t ((n' == n1 && n'' == n2) || (n' == n2 && n'' == n1))
  in
    match g with
    | (_, ed) -> has_edge ed false

  let add_edge (n1: tnode) (n2: tnode) (g: tgraph) = match g with
  | (nd, ed) -> if are_connected n1 n2 g then g else (nd, (ed @ [(n1, n2)]))

  let get_edges (n: tnode) (g: tgraph) =
    let rec get_edges' (edges: tedges) (acc: tedges) = match edges with
    | [] -> acc
    | (n1, n2) :: t -> if n1 == n || n2 == n then get_edges' t ((n1, n2) :: acc) else get_edges' t acc
  in
    match g with
    | (_, ed) -> get_edges' ed []

  let has_node (n: tnode) (g: tgraph) = match g with
  | (nd, _) -> List.mem n nd

  let get_nodes (edge: tedge) = edge
end

module StringGraph = Graph(String)