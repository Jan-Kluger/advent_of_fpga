open! Core

let%expect_test "small grid" =
  let sim = Sim.Sim_core.create_sim () in
  let lines = [ "@@@"; "@@@"; "@@@" ] in
  let hw = Sim.Sim_core.run_grid ~sim ~lines in
  let ref_ = Sim.Sim_core.reference_solve lines in
  printf "HW: %d, Ref: %d\n" hw ref_;
  [%expect {| HW: 9, Ref: 9 |}]

let%expect_test "cross pattern" =
  let sim = Sim.Sim_core.create_sim () in
  let lines = [ ".@."; "@@@"; ".@." ] in
  let hw = Sim.Sim_core.run_grid ~sim ~lines in
  let ref_ = Sim.Sim_core.reference_solve lines in
  printf "HW: %d, Ref: %d\n" hw ref_;
  [%expect {| HW: 5, Ref: 5 |}]

let%expect_test "stable 4x4" =
  let sim = Sim.Sim_core.create_sim () in
  let lines = [ "@@@@"; "@@@@"; "@@@@"; "@@@@" ] in
  let hw = Sim.Sim_core.run_grid ~sim ~lines in
  let ref_ = Sim.Sim_core.reference_solve lines in
  printf "HW: %d, Ref: %d\n" hw ref_;
  [%expect {| HW: 4, Ref: 4 |}]

let%expect_test "batch test" =
  let test_cases =
    [ [ "@" ]
    ; [ "@@"; "@@" ]
    ; [ "@@@"; "@@@"; "@@@" ]
    ; [ "@@@@"; "@@@@"; "@@@@"; "@@@@" ]
    ; [ "@@@@@"; "@@@@@"; "@@@@@"; "@@@@@"; "@@@@@" ]
    ; [ ".@."; "@@@"; ".@." ]
    ; [ "..@.."; ".@@@."; "@@@@@"; ".@@@."; "..@.." ]
    ]
  in
  List.iter test_cases ~f:(fun lines ->
    let sim = Sim.Sim_core.create_sim () in
    let hw = Sim.Sim_core.run_grid ~sim ~lines in
    let ref_ = Sim.Sim_core.reference_solve lines in
    if hw <> ref_ then printf "MISMATCH: hw=%d ref=%d\n" hw ref_);
  printf "Done\n";
  [%expect {| Done |}]
