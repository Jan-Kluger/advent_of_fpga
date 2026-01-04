open Hardcaml
open Signal

module I = Top_intf.I
module O = Top_intf.O

let create ~pipelined scope (i : _ I.t) =
  let tracker_out = 
    if pipelined then
      Core.Dial_tracker_pipelined.hierarchical scope {
        Core.Dial_tracker.I.clock = i.clock;
        clear = i.clear;
        dir = i.dir;
        num = i.num;
        valid = i.valid;
        eof = i.eof;
      }
    else
      Core.Dial_tracker.hierarchical scope {
        Core.Dial_tracker.I.clock = i.clock;
        clear = i.clear;
        dir = i.dir;
        num = i.num;
        valid = i.valid;
        eof = i.eof;
      }
  in
  
  { O.
    count = output "count_out" tracker_out.count;
    done_ = output "done_out" tracker_out.done_;
    rotation = output "rotation_out" tracker_out.rotation;
  }

let circuit ?(pipelined=false) ?(flatten=true) () =
  let scope = Scope.create ~flatten_design:flatten () in
  Circuit.create_exn ~name:"dial_zero_crossing_counter"
    (O.to_list
      (create ~pipelined scope
        (I.map2 I.port_names I.port_widths ~f:input)))
