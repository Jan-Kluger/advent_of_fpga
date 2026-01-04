open Hardcaml
open Hardcaml_waveterm

let test_example () =
  (* Test case: sequence that hits zero multiple times *)
  let test_values = [
    -50;   (* L50: 50 -> 0, lands on zero! *)
    30;    (* R30: 0 -> 30 *)
    -30;   (* L30: 30 -> 0, lands on zero! *)
    70;    (* R70: 0 -> 70 *)
    30;    (* R30: 70 -> 0 (wraps), lands on zero! *)
  ] in
  
  let sim = Cyclesim.create (Hw.Top.circuit ~flatten:true ()) in
  let waves, sim = Waveform.create sim in
  
  let ports = Sim.Sim_core.get_ports sim in
  
  (* Reset *)
  ports.clear := Bits.vdd;
  Cyclesim.cycle sim;
  ports.clear := Bits.gnd;
  
  (* Feed values *)
  List.iter (fun v ->
    ports.data := Bits.of_int ~width:8 v;
    ports.valid := Bits.vdd;
    ports.eof := Bits.gnd;
    Cyclesim.cycle sim
  ) test_values;
  
  (* Signal EOF *)
  ports.valid := Bits.gnd;
  ports.eof := Bits.vdd;
  Cyclesim.cycle sim;
  
  let result = Bits.to_int !(ports.count) in
  Printf.printf "Example test result: %d (expected 3)\n" result;
  
  (* Print waveform *)
  Waveform.print ~display_height:20 ~display_width:90 ~wave_width:2 waves;
  
  assert (result = 3)

let test_input_file () =
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
      Printf.printf "Input file result: %d\n" count
    | None ->
      failwith "Simulation did not complete"

let () =
  Printf.printf "=== Running tests ===\n\n";
  test_example ();
  Printf.printf "\n";
  test_input_file ();
  Printf.printf "\n=== All tests passed ===\n"

