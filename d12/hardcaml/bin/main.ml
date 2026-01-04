open Core
open Sim

let () =
  let filename =
    if Array.length (Sys.get_argv ()) > 1
    then (Sys.get_argv ()).(1)
    else "input.txt"
  in
  let ic = In_channel.create filename in
  let shapes, regions = Sim_core.parse_input ic in
  In_channel.close ic;
  let hw_result = Sim_core.Sim.solve shapes regions in
  Stdio.printf "Result: %d\n" hw_result
;;
