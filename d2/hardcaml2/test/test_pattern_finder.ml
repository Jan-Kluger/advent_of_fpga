open Core
open Hardcaml_waveterm

module I = Hw.Pattern_finder.I
module O = Hw.Pattern_finder.O

module IntSet = struct
  include Set.Make (Int)

  let add s x = Set.add s x
  let fold s ~init ~f = Set.fold s ~init ~f
  let to_list s = Set.to_list s
end

let pow10 n = Int.pow 10 n

(* Compute MULT(p,k) *)
let compute_mult p k =
  let pow10_pk = pow10 (p * k) in
  let pow10_p = pow10 p in
  (pow10_pk - 1) / (pow10_p - 1)
;;

(* Proper divisors of p (d where d|p and d < p) *)
let divisors_of p =
  List.filter (List.init (p - 1) ~f:(fun i -> i + 1)) ~f:(fun d -> p % d = 0)
;;

(* Check if x with p digits is "primitive" (not itself a repetition) *)
let is_primitive p x =
  let divs = divisors_of p in
  not
    (List.exists divs ~f:(fun d ->
       let mult_d = compute_mult d (p / d) in
       x % mult_d = 0
       &&
       let quotient = x / mult_d in
       let min_d_digit = if d = 1 then 1 else pow10 (d - 1) in
       let max_d_digit = pow10 d - 1 in
       quotient >= min_d_digit && quotient <= max_d_digit))
;;

let sum_invalid_in_range_part2 lo hi =
  let max_digits = String.length (string_of_int hi) + 1 in

  let collect_for_pk p k =
    let total_digits = p * k in
    if total_digits > max_digits
    then []
    else (
      let mult = compute_mult p k in
      let min_x = if p = 1 then 1 else pow10 (p - 1) in
      let max_x = pow10 p - 1 in
      let min_invalid = min_x * mult in
      if min_invalid > hi
      then []
      else (
        let x_lo = max min_x ((lo + mult - 1) / mult) in
        let x_hi = min max_x (hi / mult) in
        List.init (max 0 (x_hi - x_lo + 1)) ~f:(fun i -> x_lo + i)
        |> List.filter ~f:(fun x -> is_primitive p x)
        |> List.map ~f:(fun x -> x * mult)
        |> List.filter ~f:(fun id -> id >= lo && id <= hi)))
  in

  let rec process_k p k acc =
    let ids = collect_for_pk p k in
    if List.is_empty ids && k > 2 && p * k > max_digits
    then acc
    else (
      let acc' = List.fold ids ~init:acc ~f:(fun s id -> IntSet.add s id) in
      if p * (k + 1) > max_digits then acc' else process_k p (k + 1) acc')
  in

  let rec process_p p acc =
    if p > max_digits / 2
    then acc
    else (
      let acc' = process_k p 2 acc in
      process_p (p + 1) acc')
  in

  let invalid_ids = process_p 1 IntSet.empty in
  IntSet.fold invalid_ids ~init:0 ~f:(fun acc id -> acc + id)
;;

(* Test helpers for specific examples *)
let list_invalid_in_range_part2 lo hi =
  let max_digits = String.length (string_of_int hi) + 1 in

  let collect_for_pk p k =
    let total_digits = p * k in
    if total_digits > max_digits
    then []
    else (
      let mult = compute_mult p k in
      let min_x = if p = 1 then 1 else pow10 (p - 1) in
      let max_x = pow10 p - 1 in
      let min_invalid = min_x * mult in
      if min_invalid > hi
      then []
      else (
        let x_lo = max min_x ((lo + mult - 1) / mult) in
        let x_hi = min max_x (hi / mult) in
        List.init (max 0 (x_hi - x_lo + 1)) ~f:(fun i -> x_lo + i)
        |> List.filter ~f:(fun x -> is_primitive p x)
        |> List.map ~f:(fun x -> x * mult)
        |> List.filter ~f:(fun id -> id >= lo && id <= hi)))
  in

  let rec process_k p k acc =
    let ids = collect_for_pk p k in
    if List.is_empty ids && k > 2 && p * k > max_digits
    then acc
    else (
      let acc' = List.fold ids ~init:acc ~f:(fun s id -> IntSet.add s id) in
      if p * (k + 1) > max_digits then acc' else process_k p (k + 1) acc')
  in

  let rec process_p p acc =
    if p > max_digits / 2
    then acc
    else (
      let acc' = process_k p 2 acc in
      process_p (p + 1) acc')
  in

  let invalid_ids = process_p 1 IntSet.empty in
  IntSet.to_list invalid_ids |> List.sort ~compare:Int.compare
;;

let%expect_test "reference implementation - part 2 examples" =
  (* From problem description:
     11-22 has two invalid IDs, 11 and 22
     95-115 has two invalid IDs, 99 and 111
     998-1012 has two invalid IDs, 999 and 1010
     1188511880-1188511890 has one invalid ID, 1188511885
     222220-222224 has one invalid ID, 222222
     1698522-1698528 contains no invalid IDs
     446443-446449 has one invalid ID, 446446
     38593856-38593862 has one invalid ID, 38593859
     565653-565659 has one invalid ID, 565656
     824824821-824824827 has one invalid ID, 824824824
     2121212118-2121212124 has one invalid ID, 2121212121
  *)
  let test_cases =
    [ (11, 22), [ 11; 22 ]
    ; (95, 115), [ 99; 111 ]
    ; (998, 1012), [ 999; 1010 ]
    ; (1188511880, 1188511890), [ 1188511885 ]
    ; (222220, 222224), [ 222222 ]
    ; (1698522, 1698528), []
    ; (446443, 446449), [ 446446 ]
    ; (38593856, 38593862), [ 38593859 ]
    ; (565653, 565659), [ 565656 ]
    ; (824824821, 824824827), [ 824824824 ]
    ; (2121212118, 2121212124), [ 2121212121 ]
    ]
  in
  List.iter test_cases ~f:(fun ((lo, hi), expected) ->
    let result = list_invalid_in_range_part2 lo hi in
    if not (List.equal Int.equal result expected)
    then
      print_s
        [%message
          "Mismatch"
            (lo : int)
            (hi : int)
            (result : int list)
            (expected : int list)]
    else print_s [%message "OK" (lo : int) (hi : int) (result : int list)]);
  [%expect
    {|
    (OK (lo 11) (hi 22) (result (11 22)))
    (OK (lo 95) (hi 115) (result (99 111)))
    (OK (lo 998) (hi 1012) (result (999 1010)))
    (OK (lo 1188511880) (hi 1188511890) (result (1188511885)))
    (OK (lo 222220) (hi 222224) (result (222222)))
    (OK (lo 1698522) (hi 1698528) (result ()))
    (OK (lo 446443) (hi 446449) (result (446446)))
    (OK (lo 38593856) (hi 38593862) (result (38593859)))
    (OK (lo 565653) (hi 565659) (result (565656)))
    (OK (lo 824824821) (hi 824824827) (result (824824824)))
    (OK (lo 2121212118) (hi 2121212124) (result (2121212121)))
    |}]
;;

let%expect_test "reference sum - part 2 examples" =
  (* Sum of all invalid IDs in the examples should be 4174379265 *)
  let ranges =
    [ (11, 22)
    ; (95, 115)
    ; (998, 1012)
    ; (1188511880, 1188511890)
    ; (222220, 222224)
    ; (1698522, 1698528)
    ; (446443, 446449)
    ; (38593856, 38593862)
    ; (565653, 565659)
    ; (824824821, 824824827)
    ; (2121212118, 2121212124)
    ]
  in
  let total =
    List.fold ranges ~init:0 ~f:(fun acc (lo, hi) -> acc + sum_invalid_in_range_part2 lo hi)
  in
  print_s [%sexp (total : int)];
  [%expect {| 4174379265 |}]
;;

let%expect_test "hardware simulation - small ranges" =
  let sim = Sim.Sim_core.create_sim () in
  (* Test small ranges where we can verify by hand *)
  (* For [10, 30]: Invalid IDs are 11 (1*11) and 22 (2*11). Sum = 33 *)
  let result = Sim.Sim_core.run_single_str ~sim ~lo_str:"10" ~hi_str:"30" in
  let expected = sum_invalid_in_range_part2 10 30 in
  print_s [%message (result : int) (expected : int)];
  [%expect {| ((result 33) (expected 33)) |}]
;;

let%expect_test "hardware matches reference - small ranges" =
  let sim = Sim.Sim_core.create_sim () in
  let test_cases =
    [ "1", "20"
    ; "10", "100"
    ; "1", "1000"
    ; "11", "22"
    ; "95", "115"
    ; "998", "1012"
    ]
  in
  List.iter test_cases ~f:(fun (lo_str, hi_str) ->
    let hw_result = Sim.Sim_core.run_single_str ~sim ~lo_str ~hi_str in
    let ref_result =
      sum_invalid_in_range_part2 (Int.of_string lo_str) (Int.of_string hi_str)
    in
    if hw_result <> ref_result
    then
      print_s
        [%message
          "Mismatch"
            (lo_str : string)
            (hi_str : string)
            (hw_result : int)
            (ref_result : int)]
    else print_s [%message "OK" (lo_str : string) (hi_str : string) (hw_result : int)]);
  [%expect
    {|
    (OK (lo_str 1) (hi_str 20) (hw_result 11))
    (OK (lo_str 10) (hi_str 100) (hw_result 495))
    (OK (lo_str 1) (hi_str 1000) (hw_result 5490))
    (OK (lo_str 11) (hi_str 22) (hw_result 33))
    (OK (lo_str 95) (hi_str 115) (hw_result 210))
    (OK (lo_str 998) (hi_str 1012) (hw_result 2009))
    |}]
;;

let%expect_test "waveform" =
  let waves, sim = Sim.Sim_core.create_sim_with_waves () in
  let _result = Sim.Sim_core.run_single_str ~sim ~lo_str:"10" ~hi_str:"30" in
  Waveform.print ~display_height:20 ~display_width:80 ~wave_width:2 waves;
  [%expect {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                   │
    │                  ││      └───────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────│
    │hi                ││ 0000.│000000000000001E                                   │
    │                  ││──────┴───────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────│
    │lo                ││ 0000.│000000000000000A                                   │
    │                  ││──────┴───────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                             │
    │                  ││──────┘     └─────────────────────────────────────────────│
    │busy              ││            ┌─────────────────────────────────────────────│
    │                  ││────────────┘                                             │
    │done_             ││                                                          │
    │                  ││──────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬─────┬───────────────────────────│
    │sum               ││ 0000000000000000       │0000.│0000000000000021           │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;
