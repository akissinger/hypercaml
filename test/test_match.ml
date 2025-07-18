module Graph = Hypercaml.Graph
module Match = Hypercaml.Match

(* Helper functions for constructing test graphs *)

(* Create a simple graph with two vertices connected by an edge *)
let make_simple_graph value =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, _) = g |> Graph.add_edge ~value [v1] [v2] in
  let g = g |> Graph.set_inputs [v1] |> Graph.set_outputs [v2] in
  g

(* Create a cycle graph of 3 vertices and 3 edges *)
let make_cycle_graph () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"f" in
  let (g, v2) = g |> Graph.add_vertex ~value:"f" in
  let (g, v3) = g |> Graph.add_vertex ~value:"f" in
  let (g, _) = g |> Graph.add_edge [v1] [v2] in
  let (g, _) = g |> Graph.add_edge [v2] [v3] in
  let (g, _) = g |> Graph.add_edge [v3] [v1] in
  g

(* Create a path graph (vertices connected in a line) *)
let make_path_graph values =
  let g = Graph.make () in
  let rec add_vertices g = function
    | [] -> (g, [])
    | v :: rest ->
        let (g, vid) = g |> Graph.add_vertex ~value:v in
        let (g, rest_vids) = add_vertices g rest in
        (g, vid :: rest_vids)
  in
  let (g, vertex_ids) = add_vertices g values in
  let rec add_edges g vertices edge_num = match vertices with
    | [] | [_] -> g
    | v1 :: v2 :: rest ->
        let (g, _) = g |> Graph.add_edge ~value:("e" ^ string_of_int edge_num) [v1] [v2] in
        add_edges g (v2 :: rest) (edge_num + 1)
  in
  add_edges g vertex_ids 1

(* Create a hypergraph with scalar edges (0-ary edges) *)
let make_scalar_graph () =
  let g = Graph.make () in
  let (g, _) = g |> Graph.add_edge ~value:"scalar1" [] [] in
  let (g, _) = g |> Graph.add_edge ~value:"scalar2" [] [] in
  g

(* Create a star graph (one central vertex connected to multiple leaves) *)
let make_star_graph center_value leaf_values =
  let g = Graph.make () in
  let (g, center) = g |> Graph.add_vertex ~value:center_value in
  let rec add_leaves g leaves_remaining edge_num = match leaves_remaining with
    | [] -> g
    | leaf_val :: rest ->
        let (g, leaf) = g |> Graph.add_vertex ~value:leaf_val in
        let (g, _) = g |> Graph.add_edge ~value:("edge" ^ string_of_int edge_num) [center] [leaf] in
        add_leaves g rest (edge_num + 1)
  in
  add_leaves g leaf_values 1

(* Create a hypergraph with a proper multi-edge *)
let make_hgraph () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, v3) = g |> Graph.add_vertex in
  let (g, v4) = g |> Graph.add_vertex in
  let (g, _) = g |> Graph.add_edge ~value:"f" [v1; v2] [v3] in
  let (g, _) = g |> Graph.add_edge ~value:"g" [v3] [v4] in
  let g = g |> Graph.set_inputs [v1; v2] |> Graph.set_outputs [v4] in
  g

(* Create a larger hypergraph with a proper multi-edge *)
let make_hgraph2 () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, v3) = g |> Graph.add_vertex in
  let (g, v4) = g |> Graph.add_vertex in
  let (g, v5) = g |> Graph.add_vertex in
  let (g, _) = g |> Graph.add_edge ~value:"f" [v1; v2] [v3] in
  let (g, _) = g |> Graph.add_edge ~value:"g" [v3] [v4] in
  let (g, _) = g |> Graph.add_edge ~value:"g" [v4] [v5] in
  let g = g |> Graph.set_inputs [v1; v2] |> Graph.set_outputs [v5] in
  g

(* Create a graph with boundary inputs/outputs *)
let make_boundary_graph () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"input" in
  let (g, v2) = g |> Graph.add_vertex ~value:"middle" in
  let (g, v3) = g |> Graph.add_vertex ~value:"output" in
  let (g, _) = g |> Graph.add_edge ~value:"process" [v1] [v2] in
  let (g, _) = g |> Graph.add_edge ~value:"result" [v2] [v3] in
  let g = g |> Graph.set_inputs [v1] |> Graph.set_outputs [v3] in
  g

(* Test cases *)

(* Empty graphs should match *)
let test_empty_graphs () =
  let g1 = Graph.make () in
  let g2 = Graph.make () in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check int) "empty graphs should produce one match" 1 (Match.count matches)

(* Single vertex should match single vertex with same value *)
let test_single_vertex_match () =
  let g1 = Graph.make () in
  let (g1, _) = g1 |> Graph.add_vertex ~value:"test" in
  let g2 = Graph.make () in
  let (g2, _) = g2 |> Graph.add_vertex ~value:"test" in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check int) "single vertex should match" 1 (Match.count matches)

(* Single vertex should not match if values differ *)
let test_single_vertex_no_match () =
  let g1 = Graph.make () in
  let (g1, _) = g1 |> Graph.add_vertex ~value:"test1" in
  let g2 = Graph.make () in
  let (g2, _) = g2 |> Graph.add_vertex ~value:"test2" in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check int) "vertices with different values should not match" 0 (Match.count matches)

(* Simple edge should match *)
let test_simple_edge_match () =
  let g1 = make_simple_graph "edge1" in
  let g2 = make_simple_graph "edge1" in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "simple edges should match" true (Match.count matches > 0)

(* Simple edge should not match if edge values differ *)
let test_simple_edge_no_match () =
  let g1 = make_simple_graph "edge1" in
  let g2 = make_simple_graph "edge2" in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check int) "edges with different values should not match" 0 (Match.count matches)

(* Local isomorphism matching - vertices must have same degree *)
let test_subgraph_matching () =
  (* Create identical small graphs that should match *)
  let g1 = Graph.make () in
  let (g1, v1) = g1 |> Graph.add_vertex ~value:"A" in
  let (g1, v2) = g1 |> Graph.add_vertex ~value:"B" in
  let (g1, _) = g1 |> Graph.add_edge ~value:"e1" [v1] [v2] in
  
  let g2 = Graph.make () in
  let (g2, u1) = g2 |> Graph.add_vertex ~value:"A" in
  let (g2, u2) = g2 |> Graph.add_vertex ~value:"B" in
  let (g2, _) = g2 |> Graph.add_edge ~value:"e1" [u1] [u2] in
  
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "identical graphs should match" true (Match.count matches > 0);
  
  (* Test that degree constraints prevent matching into larger graphs *)
  let large_path = make_path_graph ["A"; "B"; "C"] in
  let small_path = make_path_graph ["A"; "B"] in
  let no_matches = Match.match_graph small_path large_path in
  Alcotest.(check int) "degree constraints prevent subgraph matching" 0 (Match.count no_matches)


(* Cycle should match itself 3 times *)
let test_cycle_self_match () =
  let cycle = make_cycle_graph () in
  let matches = Match.match_graph cycle cycle in
  Alcotest.(check bool) "cycle should match itself" true (Match.count matches = 3)

(* cycle should not match path *)
let test_cycle_path_no_match () =
  let cycle = make_cycle_graph () in
  let path = make_path_graph ["A"; "B"; "C"] in
  let matches = Match.match_graph cycle path in
  Alcotest.(check int) "cycle should not match path (different topology)" 0 (Match.count matches)

(* Scalar edges should match *)
let test_scalar_match () =
  let g1 = make_scalar_graph () in
  let g2 = make_scalar_graph () in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "scalar graphs should match" true (Match.count matches > 0)

(* Star graphs should match with same structure *)
let test_star_match () =
  let star1 = make_star_graph "center" ["leaf1"; "leaf2"] in
  let star2 = make_star_graph "center" ["leaf1"; "leaf2"] in
  let matches = Match.match_graph star1 star2 in
  Alcotest.(check bool) "star graphs should match" true (Match.count matches > 0)

(* Star graphs with different number of leaves - degree constraints *)
let test_star_different_size () =
  (* Two identical star graphs should match *)
  let star1 = make_star_graph "center" ["leaf1"; "leaf2"] in
  let star2 = make_star_graph "center" ["leaf1"; "leaf2"] in
  let matches = Match.match_graph star1 star2 in
  Alcotest.(check bool) "identical star graphs should match" true (Match.count matches > 0);
  
  (* Different sized stars should not match due to degree constraints *)
  let star3 = make_star_graph "center" ["leaf1"; "leaf2"; "leaf3"] in
  let no_matches = Match.match_graph star1 star3 in
  Alcotest.(check int) "different sized stars should not match" 0 (Match.count no_matches);
  let reverse_no_matches = Match.match_graph star3 star1 in
  Alcotest.(check int) "reverse should also not match" 0 (Match.count reverse_no_matches);
  
  (* Test with different leaf names - should not match *)
  let star4 = make_star_graph "center" ["different1"; "different2"] in
  let name_no_matches = Match.match_graph star1 star4 in
  Alcotest.(check int) "stars with different leaf names should not match" 0 (Match.count name_no_matches)

(* Boundary graphs should respect boundary constraints *)
let test_boundary_matching () =
  let g1 = make_boundary_graph () in
  let g2 = make_boundary_graph () in
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "boundary graphs should match" true (Match.count matches > 0)

(* Test isomorphism finding *)
let test_find_iso () =
  let g1 = make_cycle_graph () in
  let g2 = make_cycle_graph () in
  let iso_opt = Match.find_iso g1 g2 in
  Alcotest.(check bool) "cycles should be isomorphic" true (Option.is_some iso_opt)

(* Non-isomorphic graphs *)
let test_no_iso () =
  let cycle = make_cycle_graph () in
  let path = make_path_graph ["A"; "B"; "C"] in
  let iso_opt = Match.find_iso cycle path in
  Alcotest.(check bool) "cycle and path should not be isomorphic" true (Option.is_none iso_opt)

(* Complex hypergraph matching *)
let test_hypergraph_match () =
  let g1 = Graph.make () in
  let (g1, v1) = g1 |> Graph.add_vertex ~value:"A" in
  let (g1, v2) = g1 |> Graph.add_vertex ~value:"B" in
  let (g1, v3) = g1 |> Graph.add_vertex ~value:"C" in
  let (g1, _) = g1 |> Graph.add_edge ~value:"hyper" [v1; v2] [v3] in
  
  let g2 = Graph.make () in
  let (g2, u1) = g2 |> Graph.add_vertex ~value:"A" in
  let (g2, u2) = g2 |> Graph.add_vertex ~value:"B" in
  let (g2, u3) = g2 |> Graph.add_vertex ~value:"C" in
  let (g2, _) = g2 |> Graph.add_edge ~value:"hyper" [u1; u2] [u3] in
  
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "hypergraphs should match" true (Match.count matches > 0)

(* Test multiple matches (symmetries) *)
let test_multiple_matches () =
  let g1 = Graph.make () in
  let (g1, _) = g1 |> Graph.add_vertex ~value:"same" in
  let (g1, _) = g1 |> Graph.add_vertex ~value:"same" in
  
  let g2 = Graph.make () in
  let (g2, _) = g2 |> Graph.add_vertex ~value:"same" in
  let (g2, _) = g2 |> Graph.add_vertex ~value:"same" in
  
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "should find multiple matches due to symmetry" true (Match.count matches >= 2)

(* Mixed edge types *)
let test_mixed_edges () =
  let g1 = Graph.make () in
  let (g1, v1) = g1 |> Graph.add_vertex ~value:"A" in
  let (g1, v2) = g1 |> Graph.add_vertex ~value:"B" in
  let (g1, _) = g1 |> Graph.add_edge ~value:"normal" [v1] [v2] in
  let (g1, _) = g1 |> Graph.add_edge ~value:"scalar" [] [] in
  
  let g2 = Graph.make () in
  let (g2, u1) = g2 |> Graph.add_vertex ~value:"A" in
  let (g2, u2) = g2 |> Graph.add_vertex ~value:"B" in
  let (g2, _) = g2 |> Graph.add_edge ~value:"normal" [u1] [u2] in
  let (g2, _) = g2 |> Graph.add_edge ~value:"scalar" [] [] in
  
  let matches = Match.match_graph g1 g2 in
  Alcotest.(check bool) "graphs with mixed edge types should match" true (Match.count matches > 0)

(* Match proper hypergraph on another *)
let test_hypergraph_match_proper () =
  let g1 = make_hgraph () in
  let g2 = make_hgraph2 () in
  let matches = Match.match_graph g1 g2 in
  List.iter (fun m -> Printf.printf "Match: %s\n" (Match.string_of_match m)) (Match.seq_to_list matches);
  Alcotest.(check int) "proper hypergraphs should match" 1 (Match.count matches)

(* Collect all tests *)
let tests = [
  "empty graphs match", `Quick, test_empty_graphs;
  "single vertex match", `Quick, test_single_vertex_match;
  "single vertex no match", `Quick, test_single_vertex_no_match;
  "simple edge match", `Quick, test_simple_edge_match;
  "simple edge no match", `Quick, test_simple_edge_no_match;
  "subgraph matching", `Quick, test_subgraph_matching;
  "cycle self match", `Quick, test_cycle_self_match;
  "cycle path no match", `Quick, test_cycle_path_no_match;
  "scalar match", `Quick, test_scalar_match;
  "star match", `Quick, test_star_match;
  "star different size", `Quick, test_star_different_size;
  "boundary matching", `Quick, test_boundary_matching;
  "find isomorphism", `Quick, test_find_iso;
  "no isomorphism", `Quick, test_no_iso;
  "hypergraph match", `Quick, test_hypergraph_match;
  "multiple matches", `Quick, test_multiple_matches;
  "mixed edges", `Quick, test_mixed_edges;
  "hypergraph match proper", `Quick, test_hypergraph_match_proper;
]

let () =
  Alcotest.run "Match tests" [
    "match_graph", tests;
  ]
