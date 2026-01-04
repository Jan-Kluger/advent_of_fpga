open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a; clear : 'a; start : 'a
    ; lo : 'a [@bits 64]; hi : 'a [@bits 64] }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { done_ : 'a; busy : 'a; sum : 'a [@bits 64] }
  [@@deriving hardcaml]
end

let max_p, max_k, max_divisors = 10, 20, 4

let pow10_values = [|
  1L; 10L; 100L; 1000L; 10_000L; 100_000L; 1_000_000L; 10_000_000L;
  100_000_000L; 1_000_000_000L; 10_000_000_000L; 100_000_000_000L;
  1_000_000_000_000L; 10_000_000_000_000L; 100_000_000_000_000L;
  1_000_000_000_000_000L; 10_000_000_000_000_000L; 100_000_000_000_000_000L;
  1_000_000_000_000_000_000L; 0L |]

let compute_mult p k =
  if p * k > 19 then 0L
  else
    let num = Int64.( - ) pow10_values.(p * k) 1L in
    let den = Int64.( - ) pow10_values.(p) 1L in
    Int64.( / ) num den

let divisors_of p =
  if p <= 1 then []
  else List.filter (List.init (p - 1) ~f:(( + ) 1)) ~f:(fun d -> p % d = 0)

let mult_table = Array.init (max_p * max_k) ~f:(fun i ->
  let p, k = (i / max_k) + 1, (i % max_k) + 2 in
  if p * k <= 19 then compute_mult p k else 0L)

let primitive_check_data = Array.init (max_p + 1) ~f:(fun p ->
  if p = 0 then [] else List.map (divisors_of p) ~f:(fun d -> (d, compute_mult d (p / d))))

let div_mult_table = Array.init 11 ~f:(fun p ->
  let data = primitive_check_data.(p) in
  Array.init max_divisors ~f:(fun i ->
    if i < List.length data then snd (List.nth_exn data i) else 0L))

let div_d_table = Array.init 11 ~f:(fun p ->
  let data = primitive_check_data.(p) in
  Array.init max_divisors ~f:(fun i ->
    if i < List.length data then fst (List.nth_exn data i) else 0))

let divisor_counts = Array.init 11 ~f:(fun p -> List.length (divisors_of p))

module State = struct
  type t = Idle | Load_params | Iterate_x | Prim_div | Prim_eval | Next_k | Next_p | Done
  [@@deriving sexp_of, compare, enumerate]
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  let sm = State_machine.create (module State) spec in
  let reg w = Variable.reg spec ~width:w in

  let current_p, current_k = reg 8, reg 8 in
  let current_x, current_mult = reg 64, reg 64 in
  let min_x_reg, max_x_reg = reg 64, reg 64 in
  let lo_reg, hi_reg, accumulated_sum = reg 64, reg 64, reg 64 in
  let prim_div_idx, prim_is_primitive = reg 4, reg 1 in
  let div_bit_idx, div_quotient, div_remainder, div_divisor, div_dividend =
    reg 7, reg 64, reg 64, reg 64, reg 64 in

  let make_rom values to_sig w idx =
    mux (sel_bottom idx w) (List.map (Array.to_list values) ~f:to_sig) in

  let pow10_rom idx = make_rom pow10_values
    (fun v -> of_int64 ~width:64 (if Int64.(v > 0L) then v else 0L)) 5 idx in

  let mult_rom p k =
    let idx = ((p -:. 1) *: of_int ~width:8 max_k) +: uresize (k -:. 2) 16 in
    make_rom mult_table (of_int64 ~width:64) 8 idx in

  let flat_rom_2d table to_sig w p idx =
    let flat = Array.to_list table |> List.concat_map ~f:Array.to_list in
    let flat_idx = (p *: of_int ~width:8 max_divisors) +: uresize idx 16 in
    mux (sel_bottom flat_idx w) (List.map flat ~f:to_sig) in

  let get_div_mult p idx = flat_rom_2d div_mult_table (of_int64 ~width:64) 8 p idx in
  let get_div_d p idx = flat_rom_2d div_d_table (of_int ~width:8) 8 p idx in
  let num_divisors_for_p = make_rom divisor_counts (of_int ~width:4) 4 current_p.value in

  let pow10_p = pow10_rom current_p.value in
  let min_x_for_p = mux2 (current_p.value ==:. 1) (of_int ~width:64 1) (pow10_rom (current_p.value -:. 1)) in
  let max_x_for_p = pow10_p -:. 1 in
  let mult_for_pk = mult_rom current_p.value current_k.value -- "mult_for_pk" in
  let current_id = sel_bottom (current_x.value *: current_mult.value) 64 -- "current_id" in

  let max_digits = of_int ~width:16 19 in
  let next_k_val = current_k.value +:. 1 in
  let next_k_valid = (current_p.value *: next_k_val) <=: max_digits in
  let next_p_val = current_p.value +:. 1 in
  let next_p_valid = (next_p_val <=:. max_p) &: ((next_p_val *: of_int ~width:8 2) <=: max_digits) in

  let cur_div_mult = get_div_mult current_p.value (uresize prim_div_idx.value 8) in
  let div_complete = div_bit_idx.value ==:. 64 in

  let div_check_match =
    let cur_d = get_div_d current_p.value (uresize prim_div_idx.value 8) in
    let min_d = mux2 (cur_d ==:. 1) (of_int ~width:64 1) (pow10_rom (uresize cur_d 8 -:. 1)) in
    let max_d = pow10_rom (uresize cur_d 8) -:. 1 in
    (div_remainder.value ==:. 0) &: (div_quotient.value >=: min_d) &: (div_quotient.value <=: max_d) in

  let cur_bit_pos = of_int ~width:7 63 -: div_bit_idx.value in
  let shifted_rem =
    (sll div_remainder.value 1) |:
    uresize (mux (sel_bottom cur_bit_pos 6) (List.init 64 ~f:(bit div_dividend.value))) 64 in
  let can_subtract = shifted_rem >=: div_divisor.value in
  let new_remainder = mux2 can_subtract (shifted_rem -: div_divisor.value) shifted_rem in
  let new_quotient = mux2 can_subtract
    (div_quotient.value |: log_shift sll (of_int ~width:64 1) (uresize cur_bit_pos 6))
    div_quotient.value in

  let all_divisors_done = (prim_div_idx.value +:. 1) >=: uresize num_divisors_for_p 4 in
  let is_non_primitive = div_check_match |: (prim_is_primitive.value ==:. 0) in

  compile [ sm.switch [
    State.Idle, [
      when_ i.start [
        lo_reg <-- i.lo; hi_reg <-- i.hi;
        current_p <--. 1; current_k <--. 2; accumulated_sum <--. 0;
        sm.set_next Load_params ]];

    State.Load_params, [
      current_mult <-- mult_for_pk;
      min_x_reg <-- min_x_for_p; max_x_reg <-- max_x_for_p;
      current_x <-- min_x_for_p;
      prim_div_idx <--. 0; prim_is_primitive <--. 1;
      sm.set_next Iterate_x ];

    State.Iterate_x, [
      if_ (current_x.value >: max_x_reg.value |: (current_id >: hi_reg.value))
        [ sm.set_next Next_k ]
        [ if_ (current_id <: lo_reg.value)
            [ current_x <-- current_x.value +:. 1 ]
            [ if_ (current_p.value ==:. 1)
                [ accumulated_sum <-- accumulated_sum.value +: current_id
                ; current_x <-- current_x.value +:. 1 ]
                [ prim_div_idx <--. 0
                ; prim_is_primitive <--. 1
                ; div_bit_idx <--. 0
                ; div_quotient <--. 0
                ; div_remainder <--. 0
                ; div_dividend <-- current_x.value
                ; div_divisor <-- cur_div_mult
                ; sm.set_next Prim_div ]]]];

    State.Prim_div, [
      if_ div_complete
        [ sm.set_next Prim_eval ]
        [ div_remainder <-- new_remainder
        ; div_quotient <-- new_quotient
        ; div_bit_idx <-- div_bit_idx.value +:. 1 ]];

    State.Prim_eval, [
      when_ div_check_match [ prim_is_primitive <--. 0 ];
      if_ all_divisors_done
        [ when_ ((prim_is_primitive.value ==:. 1) &: ~:div_check_match)
            [ accumulated_sum <-- accumulated_sum.value +: current_id ]
        ; current_x <-- current_x.value +:. 1
        ; sm.set_next Iterate_x ]
        [ if_ is_non_primitive
            [ current_x <-- current_x.value +:. 1
            ; sm.set_next Iterate_x ]
            [ prim_div_idx <-- prim_div_idx.value +:. 1
            ; div_bit_idx <--. 0
            ; div_quotient <--. 0
            ; div_remainder <--. 0
            ; div_dividend <-- current_x.value
            ; div_divisor <-- get_div_mult current_p.value (uresize (prim_div_idx.value +:. 1) 8)
            ; sm.set_next Prim_div ]]];

    State.Next_k, [
      if_ next_k_valid
        [ current_k <-- next_k_val; sm.set_next Load_params ]
        [ sm.set_next Next_p ]];

    State.Next_p, [
      if_ next_p_valid
        [ current_p <-- next_p_val; current_k <--. 2; sm.set_next Load_params ]
        [ sm.set_next Done ]];

    State.Done, [ sm.set_next Idle ]]];

  { O.done_ = sm.is Done -- "done"; busy = ~:(sm.is Idle) -- "busy"; sum = accumulated_sum.value -- "sum" }

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"pattern_finder" create i
