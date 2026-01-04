open Hardcaml

let emit_to_stdout ~pipelined ~hierarchical =
  let circuit = Hw.Top.circuit ~pipelined ~flatten:(not hierarchical) () in
  Rtl.print Verilog circuit

let emit_to_file ~pipelined ~hierarchical (path : string) =
  let circuit = Hw.Top.circuit ~pipelined ~flatten:(not hierarchical) () in
  Out_channel.with_open_bin path (fun oc ->
    Rtl.output ~output_mode:(To_channel oc) Verilog circuit
  );
  Printf.printf "Verilog written to %s\n" path

let () =
  let output_file = ref None in
  let pipelined = ref false in
  let hierarchical = ref false in
  let args = [
    ("-o", Arg.String (fun s -> output_file := Some s), "Output file (default: stdout)");
    ("-pipelined", Arg.Set pipelined, "Use pipelined design");
    ("-hierarchical", Arg.Set hierarchical, "Generate hierarchical Verilog (requires separate submodule)");
  ] in
  Arg.parse args (fun _ -> ()) "emit_verilog [-o <output.v>] [-pipelined] [-hierarchical]";
  
  match !output_file with
  | None -> emit_to_stdout ~pipelined:!pipelined ~hierarchical:!hierarchical
  | Some path -> emit_to_file ~pipelined:!pipelined ~hierarchical:!hierarchical path
