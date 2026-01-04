open Core

let parse_line line = String.to_list line |> List.map ~f:(fun c -> Char.to_int c - Char.to_int '0')

let%expect_test "reference algorithm" =
  let all_9s = String.make 100 '9' in
  let all_1s_end_9 = String.make 99 '1' ^ "9" in
  let ascending = String.init 100 ~f:(fun i -> Char.of_int_exn (Char.to_int '0' + (i mod 10))) in
  
  print_s [%message "all 9s" ~result:(Sim.Sim_core.work_2 (parse_line all_9s) : int64)];
  print_s [%message "99 ones + 9" ~result:(Sim.Sim_core.work_2 (parse_line all_1s_end_9) : int64)];
  print_s [%message "ascending" ~result:(Sim.Sim_core.work_2 (parse_line ascending) : int64)];
  [%expect {|
    ("all 9s" (result 999999999999))
    ("99 ones + 9" (result 111111111119))
    (ascending (result 999999999789))
    |}]

let%expect_test "hardware - all 9s" =
  let sim = Sim.Sim_core.create_sim () in
  let line = String.make 100 '9' in
  let hw = Sim.Sim_core.run_lines ~sim ~lines:[line] in
  let ref_val = Sim.Sim_core.work_2 (parse_line line) in
  print_s [%message (hw : int64) (ref_val : int64) (Int64.(hw = ref_val) : bool)];
  [%expect {|
    ((hw 999999999999) (ref_val 999999999999)
     ("let open Int64 in hw = ref_val" true))
    |}]

let%expect_test "hardware - 99 ones ending with 9" =
  let sim = Sim.Sim_core.create_sim () in
  let line = String.make 99 '1' ^ "9" in
  let hw = Sim.Sim_core.run_lines ~sim ~lines:[line] in
  let ref_val = Sim.Sim_core.work_2 (parse_line line) in
  print_s [%message (hw : int64) (ref_val : int64) (Int64.(hw = ref_val) : bool)];
  [%expect {|
    ((hw 111111111119) (ref_val 111111111119)
     ("let open Int64 in hw = ref_val" true))
    |}]

let%expect_test "hardware - ascending digits" =
  let sim = Sim.Sim_core.create_sim () in
  let line = String.init 100 ~f:(fun i -> Char.of_int_exn (Char.to_int '0' + (i mod 10))) in
  let hw = Sim.Sim_core.run_lines ~sim ~lines:[line] in
  let ref_val = Sim.Sim_core.work_2 (parse_line line) in
  print_s [%message (hw : int64) (ref_val : int64) (Int64.(hw = ref_val) : bool)];
  [%expect {|
    ((hw 999999999789) (ref_val 999999999789)
     ("let open Int64 in hw = ref_val" true))
    |}]

let%expect_test "hardware - multiple banks" =
  let sim = Sim.Sim_core.create_sim () in
  let lines = [
    String.make 100 '9';
    String.make 99 '1' ^ "9";
  ] in
  let hw = Sim.Sim_core.run_lines ~sim ~lines in
  let ref_val = Sim.Sim_core.reference_solve (List.map ~f:parse_line lines) in
  print_s [%message (hw : int64) (ref_val : int64) (Int64.(hw = ref_val) : bool)];
  [%expect {|
    ((hw 1111111111118) (ref_val 1111111111118)
     ("let open Int64 in hw = ref_val" true))
    |}]
