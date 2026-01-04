open Hardcaml
open Signal

module Parser = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      data : 'a; [@bits 8]
      valid : 'a;
      eof : 'a;
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      zero_signal : 'a;
      eof_signal : 'a;
      rotation : 'a; [@bits 16]
    }
    [@@deriving hardcaml]
  end

  let create _scope (i : _ I.t) =
    let rotation_width = 16 in
    let fifty = Signal.of_int ~width:rotation_width 50 in
    let spec = Reg_spec.create ~clock:i.clock () in
    let rotation = wire rotation_width in
    let data_extended = sresize i.data rotation_width in

    let sum = rotation +: data_extended in

    (* wrap signed result into 0-99 *)
    let wrap_pos x = mux2 (x >=+. 100) (x -:. 100) x in
    let wrap_neg x = mux2 (x <+. 0) (x +:. 100) x in
    let sum_mod = wrap_pos (wrap_pos (wrap_neg (wrap_neg sum))) in

    let next_rotation = mux2 i.valid sum_mod rotation in
    rotation <== mux2 i.clear fifty (reg spec next_rotation);

    { O.
      zero_signal = i.valid &: (sum_mod ==:. 0);
      eof_signal = i.eof;
      rotation;
    }

  let hierarchical scope (i : _ I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"parser" create i
end

module Counter = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      input_z : 'a;
      input_eof : 'a;
    }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      output : 'a; [@bits 16]
      valid : 'a;
    }
    [@@deriving hardcaml]
  end

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let count_width = 16 in
    let count = wire count_width in

    let next_count = count +: uresize i.input_z count_width in
    count <== reg spec next_count;

    { O.
      output = count;
      valid = i.input_eof;
    }

  let hierarchical scope (i : _ I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"counter" create i
end
