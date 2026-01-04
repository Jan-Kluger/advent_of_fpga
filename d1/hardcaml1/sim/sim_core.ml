open Hardcaml

let rec parse_file (f : in_channel) : int list =
  try
    let line = String.trim (input_line f) in
    if String.length line = 0 then parse_file f
    else
      let dir = line.[0] in
      let num = int_of_string (String.sub line 1 (String.length line - 1)) in
      let value = if dir = 'L' then -(num mod 100) else (num mod 100) in
      value :: parse_file f
  with End_of_file ->
    close_in f;
    []

let load_test_data (path : string) : int list =
  parse_file (open_in path)

let create_sim () =
  Cyclesim.create (Hw.Top.circuit ~flatten:true ())

type sim_ports = {
  clear : Bits.t ref;
  data : Bits.t ref;
  valid : Bits.t ref;
  eof : Bits.t ref;
  count : Bits.t ref;
  done_ : Bits.t ref;
  rotation : Bits.t ref;
}

let get_ports sim : sim_ports =
  { clear = Cyclesim.in_port sim "rst";
    data = Cyclesim.in_port sim "data_in";
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

let feed_value sim ports value =
  ports.data := Bits.of_int ~width:8 value;
  ports.valid := Bits.vdd;
  ports.eof := Bits.gnd;
  Cyclesim.cycle sim

let signal_eof sim ports =
  ports.valid := Bits.gnd;
  ports.eof := Bits.vdd;
  Cyclesim.cycle sim

let run_simulation (values : int list) : int option =
  let sim = create_sim () in
  let ports = get_ports sim in
  
  reset sim ports;
  List.iter (fun v -> feed_value sim ports v) values;
  signal_eof sim ports;
  
  if Bits.to_int !(ports.done_) = 1 then
    Some (Bits.to_int !(ports.count))
  else
    None
