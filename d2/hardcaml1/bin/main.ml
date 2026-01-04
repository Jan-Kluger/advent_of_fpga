open! Core

let parse_input filename =
  let s = In_channel.read_all filename |> String.strip in
  String.split s ~on:','
  |> List.map ~f:(fun range ->
      match String.split range ~on:'-' with
      | [ lo; hi ] -> lo, hi
      | _ -> failwith ("Invalid range: " ^ range))
;;

let () =
  let ranges = parse_input "input.txt" in
  let sim = Sim.Sim_core.create_sim () in
  let total = Sim.Sim_core.run_ranges_str ~sim ~ranges in
  printf "Part 1: %d\n" total
;;
