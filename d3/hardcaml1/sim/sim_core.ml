open Hardcaml
open Hardcaml_waveterm

module I = Hw.Joltage_core.I
module O = Hw.Joltage_core.O

let create_sim () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  Sim.create (Hw.Joltage_core.create (Scope.create ~flatten_design:true ()))

let create_sim_with_waves () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Hw.Joltage_core.create scope) in
  Waveform.create sim

let set_inputs (inputs : Bits.t ref I.t) ~digit ~valid ~newline ~done_ =
  inputs.digit := Bits.of_int ~width:4 digit;
  inputs.digit_valid := if valid then Bits.vdd else Bits.gnd;
  inputs.newline := if newline then Bits.vdd else Bits.gnd;
  inputs.done_input := if done_ then Bits.vdd else Bits.gnd

let process_bank_str ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~line =
  let inputs = Cyclesim.inputs sim in
  String.iter (fun c ->
    set_inputs inputs ~digit:(Char.code c - Char.code '0') ~valid:true ~newline:false ~done_:false;
    Cyclesim.cycle sim
  ) line;
  set_inputs inputs ~digit:0 ~valid:false ~newline:true ~done_:false;
  Cyclesim.cycle sim

let run_lines ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~lines =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Base.List.iter lines ~f:(fun line -> 
    if String.length line > 0 then process_bank_str ~sim ~line
  );
  set_inputs inputs ~digit:0 ~valid:false ~newline:false ~done_:true;
  Cyclesim.cycle sim;
  Bits.to_int !(outputs.sum)

let run_banks ~sim ~banks =
  let lines = Base.List.map banks ~f:(fun digits ->
    String.init (List.length digits) (fun i -> Char.chr (List.nth digits i + Char.code '0'))
  ) in
  run_lines ~sim ~lines

let parse_line line = Base.String.to_list line |> Base.List.map ~f:(fun c -> Char.code c - Char.code '0')

let reference_bank_max digits =
  let rec work = function
    | x :: y :: [] -> (x, y)
    | x :: xs ->
      let ox, oy = work xs in
      let value a b = 10 * a + b in
      Base.List.fold_left ~init:(0, 0)
        ~f:(fun (ax, ay) (nx, ny) -> if value ax ay > value nx ny then (ax, ay) else (nx, ny))
        [(x, ox); (x, oy); (ox, oy)]
    | _ -> failwith "need at least 2 digits"
  in
  let t, b = work digits in
  10 * t + b

let reference_solve banks = Base.List.fold_left ~init:0 ~f:(fun acc bank -> acc + reference_bank_max bank) banks
