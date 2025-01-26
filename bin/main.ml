module G = Graph.Imperative.Digraph.Concrete (struct
  type t = Llvm.llvalue

  let compare = compare
  let equal = (==)
  let hash = Hashtbl.hash
end)

let string_of_icmp icmp =
  match icmp with
  | Llvm.Icmp.Eq -> "=="
  | Llvm.Icmp.Ne -> "!="
  | Llvm.Icmp.Ugt -> ">u"
  | Llvm.Icmp.Sgt -> ">s"
  | Llvm.Icmp.Uge -> ">=u"
  | Llvm.Icmp.Sge -> ">=s"
  | Llvm.Icmp.Ult -> "<u"
  | Llvm.Icmp.Slt -> "<s"
  | Llvm.Icmp.Ule -> "<=u"
  | Llvm.Icmp.Sle -> "<=s"

let string_of_lhs value =
  match Llvm.classify_value value with
  | Llvm.ValueKind.Argument -> Llvm.value_name value
  | _ when Llvm.is_constant value ->
    let str = 
    Llvm.string_of_llvalue value in
    let space = String.index str ' ' in
    String.sub str (space + 1) (String.length str - space - 1)
  | _ ->
    let str = Llvm.string_of_llvalue value in
    let r = Str.regexp " = " in
    let lhs = 
    (try
      let idx = Str.search_forward r str 0 in
      String.sub str 0 idx
    with Not_found ->
      raise Not_found) in
      
    String.trim lhs

let string_of_node node =
  let str = 
  (match Llvm.classify_value node with
  | Llvm.ValueKind.Instruction opc -> 
    (match opc with
    | Llvm.Opcode.Select ->
      let str = Llvm.string_of_llvalue node |> String.trim in
      let r = Str.regexp " = " in
      let idx = Str.search_forward r str 0 in
      let lhs = String.sub str 0 idx in
      let cond = Llvm.operand node 0 in
      let op1 = Llvm.operand node 1 in
      let op2 = Llvm.operand node 2 in
      Printf.sprintf "\\%s = %s ? %s : %s"
        lhs
        (string_of_lhs cond)
        (string_of_lhs op1)
        (string_of_lhs op2)
    | Llvm.Opcode.ICmp ->
      let str = Llvm.string_of_llvalue node |> String.trim in
      let r = Str.regexp " = " in
      let idx = Str.search_forward r str 0 in
      let lhs = String.sub str 0 idx in
      let pred = Llvm.icmp_predicate node |> Option.get in
      let op1 = Llvm.operand node 0 in
      let op2 = Llvm.operand node 1 in
      Printf.sprintf "\\%s = %s %s %s"
        lhs
        (string_of_lhs op1)
        (string_of_icmp pred)
        (string_of_lhs op2)
    | _ -> Llvm.string_of_llvalue node |> String.trim)
  | _ -> Llvm.string_of_llvalue node) in

  str

module DotOutput = Graph.Graphviz.Dot (struct
  include G
  let vertex_name v = "\"" ^ (string_of_node (G.V.label v)) ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = [`Fontname "monospace"]
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end)

let graph_of_function func =
  let params = Llvm.params func in
  let instrs = Llvm.fold_left_blocks (fun acc blk ->
    Llvm.fold_left_instrs (fun acc instr -> instr :: acc) acc blk
  ) [] func in

  let g = G.create () in

  params |> Array.iter (fun param -> G.add_vertex g (G.V.create param));
  instrs
  |> List.iter (fun instr -> G.add_vertex g (G.V.create instr));

  g |> G.iter_vertex (fun v ->
    let instr = G.V.label v in
    let uses = Llvm.fold_left_uses (fun acc u -> u :: acc) [] instr in
    uses |> List.iter (fun use ->
      let unode = Llvm.user use |> G.V.create in
      G.add_edge g unode v
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
