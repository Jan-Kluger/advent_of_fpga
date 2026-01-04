open Hardcaml
open Signal

(* div/mod 100 via magic constant: x/100 = (x * 5243) >> 19 *)
let div_100 x =
  let x_ext = uresize x 32 in
  let product = x_ext *: (Signal.of_int ~width:32 5243) in
  uresize (srl product 19) 16

let mod_100 x =
  let q = div_100 x in
  x -: uresize (q *: (Signal.of_int ~width:16 100)) 16

module Zero_crossing_calc = struct
  module I = struct
    type 'a t = {
      rotation : 'a; [@bits 7]
      dir : 'a; [@bits 1]
      num : 'a; [@bits 16]
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      new_rotation : 'a; [@bits 7]
      zero_crossings : 'a; [@bits 16]
    }
    [@@deriving hardcaml]
  end

  let create (i : _ I.t) =
    let rotation = uresize i.rotation 16 in
    let num = i.num in
    let is_left = i.dir ==:. 1 in
    let hundred = Signal.of_int ~width:16 100 in
    let one = Signal.of_int ~width:16 1 in
    let zero16 = Signal.of_int ~width:16 0 in
    
    let num_mod_100 = mod_100 num in
    let new_rot_l = mod_100 (rotation +: hundred -: num_mod_100) in
    let new_rot_r = mod_100 (rotation +: num_mod_100) in
    let new_rotation = mux2 is_left new_rot_l new_rot_r in
    
    let rotation_is_zero = rotation ==:. 0 in
    let num_div_100 = div_100 num in
    
    let l_crossings = mux2 rotation_is_zero
      num_div_100
      (mux2 (num >=: rotation) (one +: div_100 (num -: rotation)) zero16)
    in
    
    let hundred_minus_rotation = hundred -: rotation in
    let r_crossings = mux2 rotation_is_zero
      num_div_100
      (mux2 (num >=: hundred_minus_rotation) (one +: div_100 (num -: hundred_minus_rotation)) zero16)
    in
    
    let zero_crossings = mux2 is_left l_crossings r_crossings in
    let num_is_zero = num ==:. 0 in
    
    { O.
      new_rotation = select (mux2 num_is_zero rotation new_rotation) 6 0;
      zero_crossings = mux2 num_is_zero zero16 zero_crossings;
    }
end

module Dial_tracker = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      dir : 'a; [@bits 1]
      num : 'a; [@bits 16]
      valid : 'a;
      eof : 'a;
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      count : 'a; [@bits 16]
      done_ : 'a;
      rotation : 'a; [@bits 7]
    }
    [@@deriving hardcaml]
  end

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    let spec_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    let rotation = wire 7 in
    let count = wire 16 in
    let fifty = Signal.of_int ~width:7 50 in
    
    let calc_out = Zero_crossing_calc.create {
      Zero_crossing_calc.I.rotation;
      dir = i.dir;
      num = i.num;
    } in
    
    let next_rotation = mux2 i.valid calc_out.new_rotation rotation in
    rotation <== mux2 i.clear fifty (reg spec next_rotation);
    
    let next_count = mux2 i.valid (count +: calc_out.zero_crossings) count in
    count <== reg spec_clear next_count;
    
    { O.
      count;
      done_ = i.eof;
      rotation;
    }

  let hierarchical scope (i : _ I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"dial_tracker" create i
end

(* Pipelined variant: registers inputs for higher Fmax *)
module Dial_tracker_pipelined = struct
  module I = Dial_tracker.I
  module O = Dial_tracker.O

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    let spec_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    let dir_r = reg spec_clear i.dir in
    let num_r = reg spec_clear i.num in
    let valid_r = reg spec_clear i.valid in
    let eof_r = reg spec_clear i.eof in
    
    let rotation = wire 7 in
    let count = wire 16 in
    let fifty = Signal.of_int ~width:7 50 in
    
    let calc_out = Zero_crossing_calc.create {
      Zero_crossing_calc.I.rotation;
      dir = dir_r;
      num = num_r;
    } in
    
    let next_rotation = mux2 valid_r calc_out.new_rotation rotation in
    rotation <== mux2 i.clear fifty (reg spec next_rotation);
    
    let next_count = mux2 valid_r (count +: calc_out.zero_crossings) count in
    count <== reg spec_clear next_count;
    
    { O.
      count;
      done_ = eof_r;
      rotation;
    }

  let hierarchical scope (i : _ I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"dial_tracker_pipelined" create i
end
