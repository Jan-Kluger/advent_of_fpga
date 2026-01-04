open Hardcaml

let () =
  let module C = Circuit.With_interface (Hw.Paper_roll_part2.I) (Hw.Paper_roll_part2.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"paper_roll" (Hw.Paper_roll_part2.hierarchical scope) in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
