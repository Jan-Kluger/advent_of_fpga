open! Core

let () =
  let lines =
    In_channel.read_all "input.txt"
    |> String.strip
    |> String.split_lines
    |> List.filter ~f:(fun s -> String.length s > 0)
  in
  let sim = Sim.Sim_core.create_sim () in
  printf "%d\n" (Sim.Sim_core.run_grid ~sim ~lines)
