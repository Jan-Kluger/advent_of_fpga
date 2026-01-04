open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; digit : 'a [@bits 4]
    ; digit_valid : 'a
    ; newline : 'a
    ; done_input : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { sum : 'a [@bits 32]
    ; done_ : 'a
    ; busy : 'a
    }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  (* architectural state *)
  let best_tens = Variable.reg reg_spec ~width:4 in
  let best_val = Variable.reg reg_spec ~width:7 in
  let sum = Variable.reg reg_spec ~width:32 in
  let busy = Variable.reg reg_spec ~width:1 in

  (* pipeline registers: C -> W *)
  let candidate_p = Variable.reg reg_spec ~width:7 in
  let digit_p = Variable.reg reg_spec ~width:4 in
  let valid_p = Variable.reg reg_spec ~width:1 in
  let newline_p = Variable.reg reg_spec ~width:1 in

  let times_10 x = sll x 3 +: sll x 1 in
  let max_of a b = mux2 (a >: b) a b in

  (* stage W: compute updates from pipeline output (only when valid_p) *)
  let new_best_val = mux2 valid_p.value (max_of candidate_p.value best_val.value) best_val.value in
  let new_best_tens = mux2 valid_p.value (max_of digit_p.value best_tens.value) best_tens.value in

  (* stage C: forward best_tens, but use 0 if newline_p (we're resetting) *)
  let best_tens_fwd = 
    mux2 newline_p.value 
      (zero 4)  (* reset case: use 0 for next bank's first digit *)
      (mux2 valid_p.value new_best_tens best_tens.value)
    -- "best_tens_fwd" 
  in
  let candidate = times_10 (uresize best_tens_fwd 7) +: uresize i.digit 7 -- "candidate" in

  compile [
    (* C -> W pipeline registers *)
    candidate_p <-- candidate;
    digit_p <-- i.digit;
    valid_p <-- i.digit_valid;
    newline_p <-- i.newline;

    (* W stage: process valid digit first, then handle newline *)
    when_ valid_p.value [
      best_tens <-- new_best_tens;
      best_val <-- new_best_val;
    ];
    
    when_ newline_p.value [
      sum <-- sum.value +: uresize new_best_val 32;
      best_tens <--. 0;
      best_val <--. 0;
    ];

    when_ i.digit_valid [ busy <--. 1 ];
  ];

  { O.sum = sum.value -- "sum"; done_ = i.done_input; busy = busy.value }

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"joltage_core" create i
