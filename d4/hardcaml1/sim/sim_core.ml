open Hardcaml
open Hardcaml_waveterm

module I = Hw.Paper_roll_core.I
module O = Hw.Paper_roll_core.O

let create_sim () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  Sim.create (Hw.Paper_roll_core.create (Scope.create ~flatten_design:true ()))

let create_sim_with_waves () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Hw.Paper_roll_core.create scope) in
  Waveform.create sim

let set_inputs (inputs : Bits.t ref I.t) ~byte ~valid ~done_ ~clear =
  inputs.byte_in := Bits.of_int ~width:8 byte;
  inputs.valid := Bits.(if valid then vdd else gnd);
  inputs.done_input := Bits.(if done_ then vdd else gnd);
  inputs.clear := Bits.(if clear then vdd else gnd)

let send_byte ~sim ~byte =
  set_inputs (Cyclesim.inputs sim) ~byte ~valid:true ~done_:false ~clear:false;
  Cyclesim.cycle sim

let send_char ~sim ~c = send_byte ~sim ~byte:(Char.code c)

let send_string ~sim s = String.iter (fun c -> send_char ~sim ~c) s

let send_padding_row ~sim ~width =
  for _ = 0 to width + 1 do send_char ~sim ~c:'.' done;
  send_char ~sim ~c:'\n'

let send_line_padded ~sim ~line =
  send_char ~sim ~c:'.';
  send_string ~sim line;
  send_char ~sim ~c:'.';
  send_char ~sim ~c:'\n'

let finish ~sim ~cycles =
  let inputs = Cyclesim.inputs sim in
  set_inputs inputs ~byte:0 ~valid:false ~done_:true ~clear:false;
  Cyclesim.cycle sim;
  for _ = 1 to cycles do
    set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:false;
    Cyclesim.cycle sim
  done

let run_grid ~(sim : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t) ~lines =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:true;
  Cyclesim.cycle sim;
  set_inputs inputs ~byte:0 ~valid:false ~done_:false ~clear:false;
  Cyclesim.cycle sim;
  
  let width = Base.List.fold lines ~init:0 ~f:(fun acc line -> max acc (String.length line)) in
  send_padding_row ~sim ~width;
  Base.List.iter lines ~f:(fun line -> 
    if String.length line > 0 then send_line_padded ~sim ~line
  );
  send_padding_row ~sim ~width;
  send_padding_row ~sim ~width;
  finish ~sim ~cycles:10;
  
  Bits.to_int !(outputs.count)

let reference_solve lines =
  let height = List.length lines in
  if height = 0 then 0 else
  let width = String.length (List.hd lines) in
  let grid = Array.of_list (List.map (fun line ->
    Array.init width (fun i -> if i < String.length line && line.[i] = '@' then 1 else 0)
  ) lines) in
  let in_bounds r c = r >= 0 && r < height && c >= 0 && c < width in
  let count_neighbors row col =
    let sum = ref 0 in
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if (dr <> 0 || dc <> 0) && in_bounds (row + dr) (col + dc) then
          sum := !sum + grid.(row + dr).(col + dc)
      done
    done;
    !sum
  in
  let total = ref 0 in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      if grid.(row).(col) = 1 && count_neighbors row col < 4 then
        incr total
    done
  done;
  !total
