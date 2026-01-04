open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; lo : 'a [@bits 64]
    ; hi : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; busy : 'a
    ; sum : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

let pow10_values = [| 1; 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 |]
let multiplier_values = Array.map (fun x -> x + 1) pow10_values
let max_n = 7

module State = struct
  type t =
    | Idle
    | Iterating
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  let state_machine = State_machine.create (module State) reg_spec in
  let current_n = Variable.reg reg_spec ~width:4 in
  let current_x = Variable.reg reg_spec ~width:64 in
  let accumulated_sum = Variable.reg reg_spec ~width:64 in
  let lo_reg = Variable.reg reg_spec ~width:64 in
  let hi_reg = Variable.reg reg_spec ~width:64 in

  let pow10_rom idx =
    mux idx (Base.List.map (Array.to_list pow10_values) ~f:(fun v -> of_int ~width:64 v))
  in
  let multiplier_rom idx =
    mux idx (Base.List.map (Array.to_list multiplier_values) ~f:(fun v -> of_int ~width:64 v))
  in

  let next_n = current_n.value +:. 1 in
  let pow10_n = pow10_rom current_n.value -- "pow10_n" in
  let multiplier = multiplier_rom current_n.value -- "multiplier" in
  let multiplier_next = multiplier_rom next_n in

  let min_x_next =
    let pow10_next_n_div_10 = pow10_rom current_n.value in
    mux2 (next_n ==:. 1) (of_int ~width:64 1) pow10_next_n_div_10
  in

  let max_x = pow10_n -:. 1 -- "max_x" in

  let repeated_number =
    sel_bottom (current_x.value *: multiplier) 64 -- "repeated_number"
  in

  let in_range =
    let lo = lo_reg.value in
    let hi = hi_reg.value in
    repeated_number >=: lo &: (repeated_number <=: hi)
  in

  let need_next_n =
    let past_hi = repeated_number >: hi_reg.value in
    let x_past_max = current_x.value >: max_x in
    past_hi |: x_past_max
  in

  let all_done =
    let min_invalid_next = sel_bottom (min_x_next *: multiplier_next) 64 in
    let next_n_exceeds = min_invalid_next >: hi_reg.value in
    next_n_exceeds |: (next_n >=:. max_n)
  in

  compile
    [ state_machine.switch
        [ ( State.Idle
          , [ when_
                i.start
                [ lo_reg <-- i.lo
                ; hi_reg <-- i.hi
                ; current_n <--. 1
                ; current_x <--. 1
                ; accumulated_sum <--. 0
                ; state_machine.set_next Iterating
                ]
            ] )
        ; ( State.Iterating
          , [ if_
                need_next_n
                [ if_
                    all_done
                    [ state_machine.set_next Done ]
                    [ current_n <-- next_n; current_x <-- min_x_next ]
                ]
                [ when_ in_range [ accumulated_sum <-- accumulated_sum.value +: repeated_number ]
                ; current_x <-- current_x.value +:. 1
                ]
            ] )
        ; State.Done, [ state_machine.set_next Idle ]
        ]
    ];

  { O.done_ = state_machine.is Done -- "done"
  ; busy = ~:(state_machine.is Idle) -- "busy"
  ; sum = accumulated_sum.value -- "sum"
  }
;;

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"pattern_finder" create i
;;
