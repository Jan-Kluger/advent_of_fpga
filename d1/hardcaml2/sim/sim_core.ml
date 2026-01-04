open Hardcaml

let rec parse_file (f : in_channel) : (char * int) list =
  try
    let line = String.trim (input_line f) in
    if String.length line = 0 then parse_file f
    else
      let dir = line.[0] in
      let num = int_of_string (String.sub line 1 (String.length line - 1)) in
      (dir, num) :: parse_file f
  with End_of_file ->
    close_in f;
    []

let load_test_data (path : string) : (char * int) list =
  parse_file (open_in path)

let create_sim ?(pipelined=false) () =
  Cyclesim.create (Hw.Top.circuit ~pipelined ~flatten:true ())

type sim_ports = {
  clear : Bits.t ref;
  dir : Bits.t ref;
  num : Bits.t ref;
  valid : Bits.t ref;
  eof : Bits.t ref;
  count : Bits.t ref;
  done_ : Bits.t ref;
  rotation : Bits.t ref;
}

let get_ports sim : sim_ports =
  { clear = Cyclesim.in_port sim "rst";
    dir = Cyclesim.in_port sim "dir_in";
    num = Cyclesim.in_port sim "num_in";
    valid = Cyclesim.in_port sim "valid_in";
    eof = Cyclesim.in_port sim "eof_in";
    count = Cyclesim.out_port sim "count_out";
    done_ = Cyclesim.out_port sim "done_out";
    rotation = Cyclesim.out_port sim "rotation_out";
  }

let reset sim ports =
  ports.clear := Bits.vdd;
  Cyclesim.cycle sim;
  ports.clear := Bits.gnd

let feed_value sim ports dir num =
  let dir_bit = match dir with 'L' -> 1 | _ -> 0 in
  ports.dir := Bits.of_int ~width:1 dir_bit;
  ports.num := Bits.of_int ~width:16 num;
  ports.valid := Bits.vdd;
  ports.eof := Bits.gnd;
  Cyclesim.cycle sim

let signal_eof sim ports =
  ports.valid := Bits.gnd;
  ports.eof := Bits.vdd;
  Cyclesim.cycle sim

let run_simulation ?(pipelined=false) (values : (char * int) list) : int option =
  let sim = create_sim ~pipelined () in
  let ports = get_ports sim in
  
  reset sim ports;
  List.iter (fun (dir, num) -> feed_value sim ports dir num) values;
  signal_eof sim ports;
  
  (* For pipelined version, need extra cycles for pipeline to flush *)
  if pipelined then begin
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
  end;
  
  if Bits.to_int !(ports.done_) = 1 then
    Some (Bits.to_int !(ports.count))
  else
    None
