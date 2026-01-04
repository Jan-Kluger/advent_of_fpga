open Hardcaml

let () =
  let module C = Circuit.With_interface (Hw.Paper_roll_core.I) (Hw.Paper_roll_core.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"paper_roll_top" (Hw.Paper_roll_core.hierarchical scope) in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
