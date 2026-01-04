open Core
open Hardcaml_waveterm

let parse_line line = String.to_list line |> List.map ~f:(fun c -> Char.to_int c - Char.to_int '0')

let reference_bank_max digits =
  let rec work = function
    | x :: y :: [] -> (x, y)
    | x :: xs ->
      let ox, oy = work xs in
      let value a b = 10 * a + b in
      List.fold_left ~init:(0, 0)
        ~f:(fun (ax, ay) (nx, ny) -> if value ax ay > value nx ny then (ax, ay) else (nx, ny))
        [(x, ox); (x, oy); (ox, oy)]
    | _ -> failwith "need at least 2 digits"
  in
  let t, b = work digits in
  10 * t + b

let reference_solve banks = List.fold_left ~init:0 ~f:(fun acc bank -> acc + reference_bank_max bank) banks

let%expect_test "reference - problem examples" =
  let test s expected =
    let result = reference_bank_max (parse_line s) in
    if result <> expected then print_s [%message "FAIL" (s : string) (result : int) (expected : int)]
  in
  test "987654321111111" 98;
  test "811111111111119" 89;
  test "234234234234278" 78;
  test "818181911112111" 92;
  let banks = List.map ~f:parse_line ["987654321111111"; "811111111111119"; "234234234234278"; "818181911112111"] in
  print_s [%sexp (reference_solve banks : int)];
  [%expect {| 357 |}]

let%expect_test "hardware - single bank" =
  let sim = Sim.Sim_core.create_sim () in
  print_s [%sexp (Sim.Sim_core.run_banks ~sim ~banks:[parse_line "987654321111111"] : int)];
  [%expect {| 98 |}]

let%expect_test "hardware - problem examples" =
  let sim = Sim.Sim_core.create_sim () in
  let result = Sim.Sim_core.run_lines ~sim ~lines:["987654321111111"; "811111111111119"; "234234234234278"; "818181911112111"] in
  print_s [%sexp (result : int)];
  [%expect {| 357 |}]

let%expect_test "hardware matches reference" =
  let test_cases = ["12"; "21"; "91"; "19"; "99"; "123456789"; "987654321"; "111111119"; "911111111"] in
  List.iter test_cases ~f:(fun line ->
    let sim = Sim.Sim_core.create_sim () in
    let hw = Sim.Sim_core.run_banks ~sim ~banks:[parse_line line] in
    let ref = reference_bank_max (parse_line line) in
    if hw <> ref
    then print_s [%message "MISMATCH" (line : string) (hw : int) (ref : int)]
    else print_s [%message "OK" (line : string) (hw : int)]);
  [%expect {|
    (OK (line 12) (hw 12))
    (OK (line 21) (hw 21))
    (OK (line 91) (hw 91))
    (OK (line 19) (hw 19))
    (OK (line 99) (hw 99))
    (OK (line 123456789) (hw 89))
    (OK (line 987654321) (hw 98))
    (OK (line 111111119) (hw 19))
    (OK (line 911111111) (hw 91))
    |}]

let%expect_test "hardware - multiple banks" =
  let sim = Sim.Sim_core.create_sim () in
  let lines = ["12345"; "54321"; "99999"; "11111"; "91919"] in
  let hw = Sim.Sim_core.run_lines ~sim ~lines in
  let ref = reference_solve (List.map ~f:parse_line lines) in
  print_s [%message (hw : int) (ref : int) ((hw = ref) : bool)];
  [%expect {| ((hw 308) (ref 308) ("hw = ref" true)) |}]

let%expect_test "waveform" =
  let waves, sim = Sim.Sim_core.create_sim_with_waves () in
  let _ = Sim.Sim_core.run_lines ~sim ~lines:["123"; "91"] in
  Waveform.print ~display_height:20 ~display_width:90 ~wave_width:2 waves;
  [%expect {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │clear             ││──────┐                                                             │
    │                  ││      └───────────────────────────────────────────────              │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────              │
    │digit             ││ 0    │1    │2    │3    │0    │9    │1    │0                        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────              │
    │digit_valid       ││      ┌─────────────────┐     ┌───────────┐                         │
    │                  ││──────┘                 └─────┘           └───────────              │
    │done_input        ││                                                ┌─────              │
    │                  ││────────────────────────────────────────────────┘                   │
    │newline           ││                        ┌─────┐           ┌─────┐                   │
    │                  ││────────────────────────┘     └───────────┘     └─────              │
    │busy              ││            ┌─────────────────────────────────────────              │
    │                  ││────────────┘                                                       │
    │done_             ││                                                ┌─────              │
    │                  ││────────────────────────────────────────────────┘                   │
    │                  ││────────────────────────────────────┬─────────────────              │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    |}]
