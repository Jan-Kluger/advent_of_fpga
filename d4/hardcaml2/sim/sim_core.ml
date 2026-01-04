open Hardcaml

module I = Hw.Paper_roll_part2.I
module O = Hw.Paper_roll_part2.O

let create_sim () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  Sim.create (Hw.Paper_roll_part2.create (Scope.create ~flatten_design:true ()))

let set_inputs (inputs : Bits.t ref I.t) ~byte ~valid ~done_ ~clear =
  inputs.byte_in := Bits.of_int ~width:8 byte;
  inputs.valid := Bits.(if valid then vdd else gnd);
  inputs.done_input := Bits.(if done_ then vdd else gnd);
  inputs.clear := Bits.(if clear then vdd else gnd)

let send_char ~sim ~c =
  set_inputs (Cyclesim.inputs sim) ~byte:(Char.code c) ~valid:true ~done_:false ~clear:false;
  Cyclesim.cycle sim

let send_padding_row ~sim ~width =
  for _ = 0 to width + 1 do send_char ~sim ~c:'.' done;
  send_char ~sim ~c:'\n'

let send_line_padded ~sim ~line =
  send_char ~sim ~c:'.';
  String.iter (fun c -> send_char ~sim ~c) line;
  send_char ~sim ~c:'.';
  send_char ~sim ~c:'\n'

let run_grid ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~lines =
  let inputs = Cyclesim.inputs sim in
  let outputs : Bits.t ref O.t = Cyclesim.outputs sim in
  set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:true;
  Cyclesim.cycle sim;
  set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:false;
  Cyclesim.cycle sim;
  let width = Base.List.fold lines ~init:0 ~f:(fun acc line -> max acc (String.length line)) in
  send_padding_row ~sim ~width;
  Base.List.iter lines ~f:(fun line ->
    if String.length line > 0 then send_line_padded ~sim ~line);
  send_padding_row ~sim ~width;
  send_padding_row ~sim ~width;
  set_inputs inputs ~byte:0 ~valid:false ~done_:true ~clear:false;
  Cyclesim.cycle sim;
  let rec wait n =
    if n <= 0 || Bits.to_bool !(outputs.O.all_done) then ()
    else (set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:false;
          Cyclesim.cycle sim; wait (n - 1))
  in
  wait 1_000_000;
  Bits.to_int !(outputs.count_part2)

let reference_solve lines =
  let height = List.length lines in
  if height = 0 then 0 else
  let width = String.length (List.hd lines) in
  let alive = Array.of_list (List.map (fun line ->
    Array.init width (fun i -> i < String.length line && line.[i] = '@')
  ) lines) in
  let degree = Array.init height (fun _ -> Array.make width 0) in
  let in_bounds r c = r >= 0 && r < height && c >= 0 && c < width in
  let count_neighbors row col =
    let sum = ref 0 in
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if (dr <> 0 || dc <> 0) && in_bounds (row + dr) (col + dc) && alive.(row + dr).(col + dc) then
          incr sum
      done
    done;
    !sum
  in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      degree.(row).(col) <- count_neighbors row col
    done
  done;
  let queue = Queue.create () in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      if alive.(row).(col) && degree.(row).(col) < 4 then
        Queue.add (row, col) queue
    done
  done;
  let removed = ref 0 in
  while not (Queue.is_empty queue) do
    let (row, col) = Queue.pop queue in
    if alive.(row).(col) then begin
      alive.(row).(col) <- false;
      incr removed;
      for dr = -1 to 1 do
        for dc = -1 to 1 do
          if dr <> 0 || dc <> 0 then begin
            let nr, nc = row + dr, col + dc in
            if in_bounds nr nc && alive.(nr).(nc) then begin
              let old_deg = degree.(nr).(nc) in
              degree.(nr).(nc) <- old_deg - 1;
              if old_deg = 4 then Queue.add (nr, nc) queue
            end
          end
        done
      done
    end
  done;
  !removed
