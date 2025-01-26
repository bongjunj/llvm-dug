module G = Graph.Imperative.Digraph.Concrete (struct
  type t = Llvm.llvalue

  let compare = compare
  let equal = (==)
  let hash = Hashtbl.hash
end)

module DotOutput = Graph.Graphviz.Dot (struct
  include G
  let vertex_name v = "\"" ^ (Llvm.string_of_llvalue (G.V.label v)) ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end)

let graph_of_function func =
  let instrs = Llvm.fold_left_blocks (fun acc blk ->
    Llvm.fold_left_instrs (fun acc instr -> instr :: acc) acc blk
  ) [] func in

  let g = G.create () in

  instrs
  |> List.iter (fun instr -> G.add_vertex g (G.V.create instr));

  g |> G.iter_vertex (fun v ->
    let instr = G.V.label v in
    let uses = Llvm.fold_left_uses (fun acc u -> u :: acc) [] instr in
    uses |> List.iter (fun use ->
      let unode = Llvm.user use |> G.V.create in
      G.add_edge g v unode
      ); ()
    
    );
  
  (* set a name of the graph *)

  (Llvm.value_name func, g)


let () =
  assert (Array.length Sys.argv = 2);
  let filename = Sys.argv.(1) in
  let ctx = Llvm.global_context () in
  let membuf = Llvm.MemoryBuffer.of_file filename in
  let ir = Llvm_irreader.parse_ir ctx membuf in
  let funcs = Llvm.fold_left_functions (fun acc f -> f :: acc) [] ir in
  let graphs = funcs |> List.map graph_of_function in

  graphs |> List.iter (fun (name, g) ->
    let outfile = open_out (name ^ ".dot") in
    DotOutput.output_graph outfile g;
    close_out outfile
  )
