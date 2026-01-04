open Hardcaml

let () =
  let module C = Circuit.With_interface (Hw.Joltage_core.I) (Hw.Joltage_core.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"joltage_core_top" (Hw.Joltage_core.hierarchical scope) in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
