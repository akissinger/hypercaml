module Graph = Hypergraph.Graph

let test_make_graph () =
  let g = Graph.make () in
  Alcotest.(check int) "empty graph has 0 Graph.vertices" 0 (Graph.num_vertices g);
  Alcotest.(check int) "empty graph has 0 edges" 0 (Graph.num_edges g)

let test_add_vertex () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"test" in
  Alcotest.(check int) "g has 1 vertex" 1 (Graph.num_vertices g);
  let vdata = Graph.vertex_data g v1 in
  Alcotest.(check string) "vertex has correct value" "test" vdata.value

let test_add_edge () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"Graph.source" in
  let (g, v2) = g |> Graph.add_vertex ~value:"Graph.target" in
  let (g, e1) = g |> Graph.add_edge ~value:"test_edge" [v1] [v2] in
  Alcotest.(check int) "g has 1 edge" 1 (Graph.num_edges g);
  let edata = Graph.edge_data g e1 in
  Alcotest.(check string) "edge has correct value" "test_edge" edata.value

let test_inputs_outputs () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let g = g |> Graph.set_inputs [v1]
            |> Graph.set_outputs [v2] in
  Alcotest.(check bool) "v1 is input" true (Graph.is_input g v1);
  Alcotest.(check bool) "v2 is output" true (Graph.is_output g v2);
  Alcotest.(check bool) "v1 is boundary" true (Graph.is_boundary g v1)

let test_named_vertices () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~name:42 ~value:"named_vertex" in
  Alcotest.(check int) "vertex has correct name" 42 v1;
  let vdata = Graph.vertex_data g v1 in
  Alcotest.(check string) "named vertex has correct value" "named_vertex" vdata.value;
  Alcotest.(check int) "vindex updated correctly" 43 g.vindex

let test_named_edges () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, e1) = g |> Graph.add_edge ~name:100 ~value:"named_edge" [v1] [v2] in
  Alcotest.(check int) "edge has correct name" 100 e1;
  let edata = Graph.edge_data g e1 in
  Alcotest.(check string) "named edge has correct value" "named_edge" edata.value;
  Alcotest.(check int) "eindex updated correctly" 101 g.eindex

let test_hyperedge_multiple_vertices () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"v1" in
  let (g, v2) = g |> Graph.add_vertex ~value:"v2" in
  let (g, v3) = g |> Graph.add_vertex ~value:"v3" in
  let (g, v4) = g |> Graph.add_vertex ~value:"v4" in
  let (g, e1) = g |> Graph.add_edge ~value:"multi_edge" [v1; v2] [v3; v4] in
  
  let edata = Graph.edge_data g e1 in
  Alcotest.(check (list int)) "edge has correct Graph.sources" [v1; v2] edata.s;
  Alcotest.(check (list int)) "edge has correct Graph.targets" [v3; v4] edata.t;
  
  (* Check that Graph.source Graph.vertices have the edge in their Graph.out_edges *)
  let v1_data = Graph.vertex_data g v1 in
  let v2_data = Graph.vertex_data g v2 in
  Alcotest.(check bool) "v1 has edge in Graph.out_edges" true (Graph.NSet.mem e1 v1_data.Graph.out_edges);
  Alcotest.(check bool) "v2 has edge in Graph.out_edges" true (Graph.NSet.mem e1 v2_data.Graph.out_edges);
  
  (* Check that Graph.target Graph.vertices have the edge in their Graph.in_edges *)
  let v3_data = Graph.vertex_data g v3 in
  let v4_data = Graph.vertex_data g v4 in
  Alcotest.(check bool) "v3 has edge in Graph.in_edges" true (Graph.NSet.mem e1 v3_data.Graph.in_edges);
  Alcotest.(check bool) "v4 has edge in Graph.in_edges" true (Graph.NSet.mem e1 v4_data.Graph.in_edges)

let test_vertex_edge_lists () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, e1) = g |> Graph.add_edge [v1] [v2] in
  
  let vertex_list = Graph.vertices g in
  let edge_list = Graph.edges g in
  
  Alcotest.(check int) "correct number of Graph.vertices in list" 2 (List.length vertex_list);
  Alcotest.(check int) "correct number of edges in list" 1 (List.length edge_list);
  Alcotest.(check bool) "v1 in vertex list" true (List.mem v1 vertex_list);
  Alcotest.(check bool) "v2 in vertex list" true (List.mem v2 vertex_list);
  Alcotest.(check bool) "e1 in edge list" true (List.mem e1 edge_list)

let test_source_target_functions () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"Graph.source_v" in
  let (g, v2) = g |> Graph.add_vertex ~value:"Graph.target_v" in
  let (g, v3) = g |> Graph.add_vertex ~value:"another_source" in
  let (g, e1) = g |> Graph.add_edge ~value:"test_edge" [v1; v3] [v2] in
  
  let sources = Graph.source g e1 in
  let targets = Graph.target g e1 in
  
  Alcotest.(check (list int)) "correct Graph.source Graph.vertices" [v1; v3] sources;
  Alcotest.(check (list int)) "correct Graph.target Graph.vertices" [v2] targets

let test_in_out_edges () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, v3) = g |> Graph.add_vertex in
  let (g, e1) = g |> Graph.add_edge [v1] [v2] in
  let (g, e2) = g |> Graph.add_edge [v2] [v3] in
  let (g, e3) = g |> Graph.add_edge [v1] [v3] in
  
  (* v1 should have e1 and e3 as Graph.out_edges *)
  let v1_out = Graph.out_edges g v1 in
  Alcotest.(check int) "v1 has 2 Graph.out_edges" 2 (Graph.NSet.cardinal v1_out);
  Alcotest.(check bool) "v1 Graph.out_edges contains e1" true (Graph.NSet.mem e1 v1_out);
  Alcotest.(check bool) "v1 Graph.out_edges contains e3" true (Graph.NSet.mem e3 v1_out);
  
  (* v2 should have e1 as in_edge and e2 as out_edge *)
  let v2_in = Graph.in_edges g v2 in
  let v2_out = Graph.out_edges g v2 in
  Alcotest.(check int) "v2 has 1 in_edge" 1 (Graph.NSet.cardinal v2_in);
  Alcotest.(check int) "v2 has 1 out_edge" 1 (Graph.NSet.cardinal v2_out);
  Alcotest.(check bool) "v2 Graph.in_edges contains e1" true (Graph.NSet.mem e1 v2_in);
  Alcotest.(check bool) "v2 Graph.out_edges contains e2" true (Graph.NSet.mem e2 v2_out);
  
  (* v3 should have e2 and e3 as Graph.in_edges *)
  let v3_in = Graph.in_edges g v3 in
  Alcotest.(check int) "v3 has 2 Graph.in_edges" 2 (Graph.NSet.cardinal v3_in);
  Alcotest.(check bool) "v3 Graph.in_edges contains e2" true (Graph.NSet.mem e2 v3_in);
  Alcotest.(check bool) "v3 Graph.in_edges contains e3" true (Graph.NSet.mem e3 v3_in)

let test_complex_inputs_outputs () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex ~value:"input1" in
  let (g, v2) = g |> Graph.add_vertex ~value:"input2" in
  let (g, v3) = g |> Graph.add_vertex ~value:"output1" in
  let (g, v4) = g |> Graph.add_vertex ~value:"output2" in
  let (g, v5) = g |> Graph.add_vertex ~value:"internal" in
  
  let g = g |> Graph.set_inputs [v1; v2]
            |> Graph.set_outputs [v3; v4] in
  
  (* Test Graph.inputs *)
  let input_list = Graph.inputs g in
  Alcotest.(check (list int)) "correct input list" [v1; v2] input_list;
  Alcotest.(check bool) "v1 is input" true (Graph.is_input g v1);
  Alcotest.(check bool) "v2 is input" true (Graph.is_input g v2);
  Alcotest.(check bool) "v5 is not input" false (Graph.is_input g v5);
  
  (* Test outputs *)
  let output_list = Graph.outputs g in
  Alcotest.(check (list int)) "correct output list" [v3; v4] output_list;
  Alcotest.(check bool) "v3 is output" true (Graph.is_output g v3);
  Alcotest.(check bool) "v4 is output" true (Graph.is_output g v4);
  Alcotest.(check bool) "v5 is not output" false (Graph.is_output g v5);
  
  (* Test boundary *)
  Alcotest.(check bool) "v1 is boundary" true (Graph.is_boundary g v1);
  Alcotest.(check bool) "v2 is boundary" true (Graph.is_boundary g v2);
  Alcotest.(check bool) "v3 is boundary" true (Graph.is_boundary g v3);
  Alcotest.(check bool) "v4 is boundary" true (Graph.is_boundary g v4);
  Alcotest.(check bool) "v5 is not boundary" false (Graph.is_boundary g v5)

let test_input_output_indices () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, v3) = g |> Graph.add_vertex in
  
  let g = g |> Graph.set_inputs [v1; v2; v1]  (* v1 appears at indices 0 and 2 *)
            |> Graph.set_outputs [v2; v3] in  (* v2 at index 0, v3 at index 1 *)
  
  let v1_data = Graph.vertex_data g v1 in
  let v2_data = Graph.vertex_data g v2 in
  let v3_data = Graph.vertex_data g v3 in
  
  (* Check input indices *)
  Alcotest.(check bool) "v1 has input index 0" true (Graph.NSet.mem 0 v1_data.in_indices);
  Alcotest.(check bool) "v1 has input index 2" true (Graph.NSet.mem 2 v1_data.in_indices);
  Alcotest.(check bool) "v2 has input index 1" true (Graph.NSet.mem 1 v2_data.in_indices);
  
  (* Check output indices *)
  Alcotest.(check bool) "v2 has output index 0" true (Graph.NSet.mem 0 v2_data.out_indices);
  Alcotest.(check bool) "v3 has output index 1" true (Graph.NSet.mem 1 v3_data.out_indices);
  Alcotest.(check bool) "v1 has no output indices" true (Graph.NSet.is_empty v1_data.out_indices)

let test_error_handling () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, _e1) = g |> Graph.add_edge [v1] [v1] in  (* self-loop *)
  
  (* Test accessing non-existent vertex *)
  Alcotest.check_raises "non-existent vertex raises exception" 
    (Graph.Graph_error "Vertex 999 not found") 
    (fun () -> ignore (Graph.vertex_data g 999));
  
  (* Test accessing non-existent edge *)
  Alcotest.check_raises "non-existent edge raises exception" 
    (Graph.Graph_error "Edge 999 not found") 
    (fun () -> ignore (Graph.edge_data g 999))

let test_empty_edge () =
  let g = Graph.make () in
  let (g, e1) = g |> Graph.add_edge ~value:"empty_edge" [] [] in
  
  let edata = Graph.edge_data g e1 in
  Alcotest.(check (list int)) "empty edge has no Graph.sources" [] edata.s;
  Alcotest.(check (list int)) "empty edge has no Graph.targets" [] edata.t;
  Alcotest.(check string) "empty edge has correct value" "empty_edge" edata.value

let test_string_of_edata () =
  let edata = Graph.make_edata ~s:[1; 2] ~t:[3] ~value:"test_edge" () in
  let str_repr = Graph.string_of_edata edata in
  let expected = "Edge: test_edge ([1; 2], [3])" in
  Alcotest.(check string) "string representation is correct" expected str_repr

let test_update_inputs_outputs () =
  let g = Graph.make () in
  let (g, v1) = g |> Graph.add_vertex in
  let (g, v2) = g |> Graph.add_vertex in
  let (g, v3) = g |> Graph.add_vertex in
  
  (* Set initial Graph.inputs and outputs *)
  let g = g |> Graph.set_inputs [v1]
            |> Graph.set_outputs [v2] in
  
  (* Update Graph.inputs and outputs *)
  let g = g |> Graph.set_inputs [v2; v3]
            |> Graph.set_outputs [v1] in
  
  (* Check that old indices are cleared *)
  Alcotest.(check bool) "v1 is no longer input" false (Graph.is_input g v1);
  Alcotest.(check bool) "v2 is no longer output" false (Graph.is_output g v2);
  
  (* Check new Graph.inputs and outputs *)
  Alcotest.(check bool) "v2 is now input" true (Graph.is_input g v2);
  Alcotest.(check bool) "v3 is now input" true (Graph.is_input g v3);
  Alcotest.(check bool) "v1 is now output" true (Graph.is_output g v1)

(* Test suite definition *)
let graph_tests = [
  "make_graph", `Quick, test_make_graph;
  "add_vertex", `Quick, test_add_vertex;
  "add_edge", `Quick, test_add_edge;
  "inputs_outputs", `Quick, test_inputs_outputs;
  "named_vertices", `Quick, test_named_vertices;
  "named_edges", `Quick, test_named_edges;
  "hyperedge_multiple_vertices", `Quick, test_hyperedge_multiple_vertices;
  "vertex_edge_lists", `Quick, test_vertex_edge_lists;
  "source_target_functions", `Quick, test_source_target_functions;
  "in_out_edges", `Quick, test_in_out_edges;
  "complex_inputs_outputs", `Quick, test_complex_inputs_outputs;
  "input_output_indices", `Quick, test_input_output_indices;
  "error_handling", `Quick, test_error_handling;
  "empty_edge", `Quick, test_empty_edge;
  "string_of_edata", `Quick, test_string_of_edata;
  "update_inputs_outputs", `Quick, test_update_inputs_outputs;
]

(* Run the tests *)
let () = Alcotest.run "Graph Tests" [
  "graph", graph_tests;
]