open Hardcaml

let emit_to_stdout ~hierarchical =
  let circuit = Hw.Top.circuit ~flatten:(not hierarchical) () in
  Rtl.print Verilog circuit

let emit_to_file ~hierarchical (path : string) =
  let circuit = Hw.Top.circuit ~flatten:(not hierarchical) () in
  Out_channel.with_open_bin path (fun oc ->
    Rtl.output ~output_mode:(To_channel oc) Verilog circuit
  );
  Printf.printf "Verilog written to %s\n" path

let () =
  let output_file = ref None in
  let hierarchical = ref false in
  let args = [
    ("-o", Arg.String (fun s -> output_file := Some s), "Output file (default: stdout)");
    ("-hierarchical", Arg.Set hierarchical, "Generate hierarchical Verilog");
  ] in
  Arg.parse args (fun _ -> ()) "emit_verilog [-o <output.v>] [-hierarchical]";
  
  match !output_file with
  | None -> emit_to_stdout ~hierarchical:!hierarchical
  | Some path -> emit_to_file ~hierarchical:!hierarchical path
