open Hardcaml
open Hardcaml_waveterm

module I = Hw.Pattern_finder.I
module O = Hw.Pattern_finder.O

let create_sim () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Hw.Pattern_finder.create scope)
;;

let create_sim_with_waves () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Hw.Pattern_finder.create scope)
  in
  let waves, sim = Waveform.create sim in
  waves, sim
;;

let run_single_str ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~lo_str ~hi_str =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let lo = Int64.of_string lo_str in
  let hi = Int64.of_string hi_str in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.lo := Bits.of_int64 ~width:64 lo;
  inputs.hi := Bits.of_int64 ~width:64 hi;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let max_cycles = 10_000_000 in
  let rec wait n =
    if n > max_cycles
    then failwith "Simulation timeout"
    else if Bits.is_vdd !(outputs.done_)
    then ()
    else (
      Cyclesim.cycle sim;
      wait (n + 1))
  in
  wait 0;
  Bits.to_int64 !(outputs.sum) |> Int64.to_int
;;

let run_ranges_str ~sim ~ranges =
  Base.List.fold ranges ~init:0 ~f:(fun acc (lo_str, hi_str) ->
    let result = run_single_str ~sim ~lo_str ~hi_str in
    acc + result)
;;
