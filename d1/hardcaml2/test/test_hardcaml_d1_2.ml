open Hardcaml
open Hardcaml_waveterm

let test_example () =
  (* Test case from problem description *)
  let test_values = [
    ('L', 68); ('L', 30); ('R', 48); ('L', 5); ('R', 60);
    ('L', 55); ('L', 1); ('L', 99); ('R', 14); ('L', 82);
  ] in
  
  let sim = Cyclesim.create (Hw.Top.circuit ~flatten:true ()) in
  let waves, sim = Waveform.create sim in
  
  let ports = Sim.Sim_core.get_ports sim in
  
  (* Reset *)
  ports.clear := Bits.vdd;
  Cyclesim.cycle sim;
  ports.clear := Bits.gnd;
  
  (* Feed values *)
  List.iter (fun (dir, num) ->
    let dir_bit = match dir with 'L' -> 1 | _ -> 0 in
    ports.dir := Bits.of_int ~width:1 dir_bit;
    ports.num := Bits.of_int ~width:16 num;
    ports.valid := Bits.vdd;
    ports.eof := Bits.gnd;
    Cyclesim.cycle sim
  ) test_values;
  
  (* Signal EOF *)
  ports.valid := Bits.gnd;
  ports.eof := Bits.vdd;
  Cyclesim.cycle sim;
  
  let result = Bits.to_int !(ports.count) in
  Printf.printf "Example test result: %d (expected 6)\n" result;
  
  (* Print waveform *)
  Waveform.print ~display_height:25 ~display_width:100 ~wave_width:2 waves;
  
  assert (result = 6)

let test_input_file () =
  (* Try different paths depending on where test is run from *)
  let input_paths = ["input.txt"; "../input.txt"; "../../input.txt"] in
  let rec find_input = function
    | [] -> None
    | p :: rest -> if Sys.file_exists p then Some p else find_input rest
  in
  match find_input input_paths with
  | None ->
    Printf.printf "Skipping input file test (input.txt not found)\n"
  | Some path ->
    let values = Sim.Sim_core.load_test_data path in
    match Sim.Sim_core.run_simulation values with
    | Some count ->
      Printf.printf "Input file result: %d (expected 6166)\n" count;
      assert (count = 6166)
    | None ->
      failwith "Simulation did not complete"

let test_pipelined () =
  let input_paths = ["input.txt"; "../input.txt"; "../../input.txt"] in
  let rec find_input = function
    | [] -> None
    | p :: rest -> if Sys.file_exists p then Some p else find_input rest
  in
  match find_input input_paths with
  | None ->
    Printf.printf "Skipping pipelined test (input.txt not found)\n"
  | Some path ->
    let values = Sim.Sim_core.load_test_data path in
    match Sim.Sim_core.run_simulation ~pipelined:true values with
    | Some count ->
      Printf.printf "Pipelined result: %d (expected 6166)\n" count;
      assert (count = 6166)
    | None ->
      failwith "Pipelined simulation did not complete"

let () =
  Printf.printf "=== Running tests ===\n\n";
  test_example ();
  Printf.printf "\n";
  test_input_file ();
  Printf.printf "\n";
  test_pipelined ();
  Printf.printf "\n=== All tests passed ===\n"

