open Hardcaml
open Signal

module I = Top_intf.I
module O = Top_intf.O

let create scope (i : _ I.t) =
  let parser_out = Core.Parser.hierarchical scope {
    Core.Parser.I.clock = i.clock;
    clear = i.clear;
    data = i.data;
    valid = i.valid;
    eof = i.eof;
  } in
  
  let counter_out = Core.Counter.hierarchical scope {
    Core.Counter.I.clock = i.clock;
    clear = i.clear;
    input_z = parser_out.zero_signal;
    input_eof = parser_out.eof_signal;
  } in
  
  { O.
    count = output "count_out" counter_out.output;
    done_ = output "done_out" counter_out.valid;
    rotation = output "rotation_out" parser_out.rotation;
  }

let circuit ?(flatten=true) () =
  let scope = Scope.create ~flatten_design:flatten () in
  Circuit.create_exn ~name:"dial_zero_counter"
    (O.to_list
      (create scope
        (I.map2 I.port_names I.port_widths ~f:input)))
