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

let set_inputs (inputs : Bits.t ref I.t) ~digit ~valid =
  inputs.digit := Bits.of_int ~width:4 digit;
  inputs.digit_valid := if valid then Bits.vdd else Bits.gnd

let process_bank_str ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~line =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  String.iter (fun c ->
    set_inputs inputs ~digit:(Char.code c - Char.code '0') ~valid:true;
    Cyclesim.cycle sim;
    set_inputs inputs ~digit:0 ~valid:false;
    while Bits.to_int !((outputs).busy) = 1 && Bits.to_int !((outputs).bank_done) = 0 do
      Cyclesim.cycle sim
    done
  ) line;
  while Bits.to_int !((outputs).bank_done) = 0 do
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim

let run_lines ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~lines =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Base.List.iter lines ~f:(fun line -> 
    if String.length line = Hw.Joltage_core.input_length then 
      process_bank_str ~sim ~line
  );
  Bits.to_int64 !(outputs.sum)

let run_banks ~sim ~banks =
  let lines = Base.List.map banks ~f:(fun digits ->
    String.init (List.length digits) (fun i -> Char.chr (List.nth digits i + Char.code '0'))
  ) in
  run_lines ~sim ~lines

let parse_line line = 
  Base.String.to_list line |> Base.List.map ~f:(fun c -> Char.code c - Char.code '0')

let work_2 digits =
  let n = List.length digits in
  let to_remove = n - 12 in
  let rec build_stack stack removed remaining =
    match remaining with
    | [] -> stack
    | d :: rest ->
      let rec pop_smaller stk rem =
        if rem >= to_remove then (stk, rem)
        else match stk with
          | top :: rest_stk when d > top -> pop_smaller rest_stk (rem + 1)
          | _ -> (stk, rem)
      in
      let new_stack, new_removed = pop_smaller stack removed in
      build_stack (d :: new_stack) new_removed rest
  in
  let stack = build_stack [] 0 digits in
  let result_digits = List.rev stack in
  let rec take n lst = 
    if n = 0 then [] 
    else match lst with [] -> [] | x :: xs -> x :: take (n-1) xs 
  in
  let first_12 = take 12 result_digits in
  Base.List.fold_left ~init:Int64.zero ~f:(fun acc d -> Int64.add (Int64.mul acc 10L) (Int64.of_int d)) first_12

let reference_solve banks = 
  Base.List.fold_left ~init:Int64.zero ~f:(fun acc bank -> Int64.add acc (work_2 bank)) banks
