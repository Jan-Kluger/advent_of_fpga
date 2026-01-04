open Hardcaml

let () =
  let module C = Circuit.With_interface (Hw.Pattern_finder.I) (Hw.Pattern_finder.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit =
    C.create_exn ~name:"pattern_finder_top" (Hw.Pattern_finder.hierarchical scope)
  in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
;;
