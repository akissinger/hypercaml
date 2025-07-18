This library implements bounded, directed hypergraphs and subgraph matching in OCaml. It is a port of the Python hypergraph object and matching algorithm from the [Chyp](https://github.com/akissinger/chyp) theorem prover.

Here is an example of using the library to create a hypergraph and match it against another:

```ocaml
module Graph = Hypercaml.Graph
module Match = Hypercaml.Match

let g = Graph.make () in
let (g, v1) = g |> Graph.add_vertex in
let (g, v2) = g |> Graph.add_vertex in
let (g, v3) = g |> Graph.add_vertex in
let (g, v4) = g |> Graph.add_vertex in
let (g, _) = g |> Graph.add_edge ~value:"f" [v1; v2] [v3] in
let (g, _) = g |> Graph.add_edge ~value:"g" [v3] [v4] in
let g = g |> Graph.set_inputs [v1; v2] |> Graph.set_outputs [v4];;

let h = Graph.make () in
let (h, v1) = g |> Graph.add_vertex in
let (h, v2) = g |> Graph.add_vertex in
let (h, v3) = g |> Graph.add_vertex in
let (h, v4) = g |> Graph.add_vertex in
let (h, v5) = g |> Graph.add_vertex in
let (h, _) = g |> Graph.add_edge ~value:"f" [v1; v2] [v3] in
let (h, _) = g |> Graph.add_edge ~value:"g" [v3] [v4] in
let (h, _) = g |> Graph.add_edge ~value:"g" [v4] [v5] in
let h = h |> Graph.set_inputs [v1; v2] |> Graph.set_outputs [v5];;

let matches = Match.match_graph g h;;

match Match.next_match matches with
| None -> Printf.printf "No matches found.\n"
| Some (m, matches1) -> Printf.printf "Found a match: %s\n" (Match.string_of_match m);;
```

For more examples, see [test_match.ml](test/test_match.ml).