let () =
  let input_file = ref "input.txt" in
  let args = [
    ("-i", Arg.String (fun s -> input_file := s), "Input file (default: input.txt)");
  ] in
  Arg.parse args (fun _ -> ()) "main [-i <input.txt>]";
  
  let values = Sim.Sim_core.load_test_data !input_file in
  match Sim.Sim_core.run_simulation values with
  | Some count ->
    Printf.printf "Result: %d\n" count
  | None ->
    Printf.printf "Simulation did not complete\n";
    exit 1
