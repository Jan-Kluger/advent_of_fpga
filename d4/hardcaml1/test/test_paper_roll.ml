open Core

let%expect_test "reference - problem example" =
  let lines = [
    "..@@.@@@@.";
    "@@@.@.@.@@";
    "@@@@@.@.@@";
    "@.@@@@..@.";
    "@@.@@@@.@@";
    ".@@@@@@@.@";
    ".@.@.@.@@@";
    "@.@@@.@@@@";
    ".@@@@@@@@.";
    "@.@.@@@.@.";
  ] in
  let result = Sim.Sim_core.reference_solve lines in
  print_s [%sexp (result : int)];
  [%expect {| 13 |}]

let%expect_test "reference - simple cases" =
  let test lines expected =
    let result = Sim.Sim_core.reference_solve lines in
    if result <> expected then
      print_s [%message "FAIL" (lines : string list) (result : int) (expected : int)]
    else
      print_s [%message "OK" (result : int)]
  in
  test ["@"] 1;
  test ["@@"] 2;
  test ["@@@"] 3;
  test ["@"; "@"] 2;
  test ["@"; "@"; "@"] 3;
  test ["@@"; "@@"] 4;
  test ["@@@"; "@@@"; "@@@"] 4;
  test [".@."; "@@@"; ".@."] 4;
  [%expect {|
    (OK (result 1))
    (OK (result 2))
    (OK (result 3))
    (OK (result 2))
    (OK (result 3))
    (OK (result 4))
    (OK (result 4))
    (OK (result 4))
    |}]

let%expect_test "hardware matches reference - batch" =
  let test_cases = [
    ["@"];
    ["@@"];
    ["@@@"];
    ["@"; "@"];
    ["@"; "@"; "@"];
    ["@@"; "@@"];
    ["@@@"; "@@@"; "@@@"];
    [".@."; "@@@"; ".@."];
    ["@@@@@"; "@@@@@"; "@@@@@"; "@@@@@"; "@@@@@"];
  ] in
  List.iter test_cases ~f:(fun lines ->
    let sim = Sim.Sim_core.create_sim () in
    let hw = Sim.Sim_core.run_grid ~sim ~lines in
    let ref = Sim.Sim_core.reference_solve lines in
    if hw <> ref then
      print_s [%message "MISMATCH" (lines : string list) (hw : int) (ref : int)]
    else
      print_s [%message "OK" (hw : int)]
  );
  [%expect {|
    (OK (hw 1))
    (OK (hw 2))
    (OK (hw 3))
    (OK (hw 2))
    (OK (hw 3))
    (OK (hw 4))
    (OK (hw 4))
    (OK (hw 4))
    (OK (hw 4))
    |}]
