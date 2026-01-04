open Core
open Hardcaml_waveterm

module I = Hw.Pattern_finder.I
module O = Hw.Pattern_finder.O

let sum_invalid_in_range lo hi =
  let pow10 n = Int.pow 10 n in
  let rec process n acc =
    let pow10_n = pow10 n in
    let multiplier = pow10_n + 1 in
    let min_x = if n = 1 then 1 else pow10_n / 10 in
    let max_x = pow10_n - 1 in
    let min_invalid = min_x * multiplier in
    if min_invalid > hi
    then acc
    else (
      let x_lo = max min_x ((lo + multiplier - 1) / multiplier) in
      let x_hi = min max_x (hi / multiplier) in
      let sum =
        if x_lo > x_hi
        then 0
        else (
          let count = x_hi - x_lo + 1 in
          multiplier * count * (x_lo + x_hi) / 2)
      in
      process (n + 1) (acc + sum))
  in
  process 1 0
;;

let%expect_test "reference implementation" =
  print_s [%sexp (sum_invalid_in_range 10 100 : int)];
  [%expect {| 495 |}];
  print_s [%sexp (sum_invalid_in_range 1 20 : int)];
  [%expect {| 22 |}]
;;

let%expect_test "hardware simulation" =
  let sim = Sim.Sim_core.create_sim () in
  let result = Sim.Sim_core.run_single_str ~sim ~lo_str:"10" ~hi_str:"100" in
  print_s [%sexp (result : int)];
  [%expect {| 495 |}]
;;

let%expect_test "hardware matches reference" =
  let sim = Sim.Sim_core.create_sim () in
  let test_cases = [ "1", "20"; "10", "100"; "1", "1000"; "100", "10000" ] in
  List.iter test_cases ~f:(fun (lo_str, hi_str) ->
    let hw_result = Sim.Sim_core.run_single_str ~sim ~lo_str ~hi_str in
    let ref_result = sum_invalid_in_range (Int.of_string lo_str) (Int.of_string hi_str) in
    if hw_result <> ref_result
    then
      print_s
        [%message "Mismatch" (lo_str : string) (hi_str : string) (hw_result : int) (ref_result : int)]
    else print_s [%message "OK" (lo_str : string) (hi_str : string) (hw_result : int)]);
  [%expect
    {|
    (OK (lo_str 1) (hi_str 20) (hw_result 22))
    (OK (lo_str 10) (hi_str 100) (hw_result 495))
    (OK (lo_str 1) (hi_str 1000) (hw_result 54483))
    (OK (lo_str 100) (hi_str 10000) (hw_result 109395))
    |}]
;;

let%expect_test "waveform" =
  let waves, sim = Sim.Sim_core.create_sim_with_waves () in
  let _result = Sim.Sim_core.run_single_str ~sim ~lo_str:"10" ~hi_str:"100" in
  Waveform.print ~display_height:20 ~display_width:80 ~wave_width:2 waves;
  [%expect.unreachable]
[@@expect.uncaught_exn {| (* waveform output *) |}]
;;
