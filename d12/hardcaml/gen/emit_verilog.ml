open Hardcaml
open Hw

let () =
  let module I = Present_packer_core.I in
  let module O = Present_packer_core.O in
  let scope = Scope.create ~flatten_design:false () in
  let inputs = I.map2 I.port_names I.port_widths ~f:Signal.input in
  let outputs = Present_packer_core.create scope inputs in
  let circuit =
    Circuit.create_exn
      ~name:"present_packer"
      (O.to_list (O.map2 O.port_names outputs ~f:Signal.output))
  in
  Rtl.print Verilog circuit
;;
