open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; byte_in : 'a [@bits 8]
    ; valid : 'a
    ; done_input : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { count : 'a [@bits 32]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

module Config = struct
  let max_width = 160
  let col_bits = 8
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  let is_at = i.byte_in ==:. Char.code '@' in
  let is_newline = i.byte_in ==:. Char.code '\n' in

  let padded_width = Variable.reg reg_spec ~width:Config.col_bits in
  let width_known = Variable.reg reg_spec ~width:1 in
  let in_col = Variable.reg reg_spec ~width:Config.col_bits in
  let in_row = Variable.reg reg_spec ~width:8 in
  let count = Variable.reg reg_spec ~width:32 in
  let done_reg = Variable.reg reg_spec ~width:1 in

  let lb_rd_addr = Variable.wire ~default:(zero Config.col_bits) in
  let lb_wr_addr = Variable.wire ~default:(zero Config.col_bits) in
  let lb1_wr_en = Variable.wire ~default:gnd in
  let lb1_wr_data = Variable.wire ~default:gnd in
  let lb2_wr_en = Variable.wire ~default:gnd in
  let lb2_wr_data = Variable.wire ~default:gnd in

  let make_line_buffer wr_data wr_en =
    Ram.create ~collision_mode:Read_before_write ~size:Config.max_width
      ~write_ports:[|{ write_clock = i.clock; write_address = lb_wr_addr.value;
                       write_data = wr_data; write_enable = wr_en }|]
      ~read_ports:[|{ read_clock = i.clock; read_address = lb_rd_addr.value; read_enable = vdd }|]
      ()
  in
  let line_buf_1 = make_line_buffer lb1_wr_data.value lb1_wr_en.value in
  let line_buf_2 = make_line_buffer lb2_wr_data.value lb2_wr_en.value in

  let make_window_row () = Array.init 3 (fun _ -> Variable.reg reg_spec ~width:1) in
  let win_r0 = make_window_row () in
  let win_r1 = make_window_row () in
  let win_r2 = make_window_row () in

  let p1_col = Variable.reg reg_spec ~width:Config.col_bits in
  let p1_row = Variable.reg reg_spec ~width:8 in
  let p1_cell = Variable.reg reg_spec ~width:1 in
  let p1_valid = Variable.reg reg_spec ~width:1 in
  let p2_col = Variable.reg reg_spec ~width:Config.col_bits in
  let p2_row = Variable.reg reg_spec ~width:8 in
  let p2_valid = Variable.reg reg_spec ~width:1 in

  let lb1_masked = mux2 (p1_row.value ==:. 0) gnd line_buf_1.(0) in
  let lb2_masked = mux2 (p1_row.value <=:. 1) gnd line_buf_2.(0) in

  let eval_col = p2_col.value -:. 1 in
  let in_real_columns =
    (eval_col >=:. 1) &: (eval_col <: padded_width.value -:. 1)
  in
  let eval_in_bounds = 
    (p2_row.value >=:. 1) &: (p2_col.value >=:. 1) &: in_real_columns -- "eval_in_bounds"
  in

  let center = win_r1.(1).value in
  let neighbors = [
    win_r2.(2).value; win_r2.(1).value; win_r2.(0).value;
    win_r1.(2).value;                   win_r1.(0).value;
    win_r0.(2).value; win_r0.(1).value; win_r0.(0).value;
  ] in
  let neighbor_count =
    tree ~arity:2 ~f:(reduce ~f:( +: )) (List.map (fun x -> uresize x 4) neighbors)
    -- "neighbor_count"
  in
  let accessible = p2_valid.value &: eval_in_bounds &: center &: (neighbor_count <:. 4) -- "accessible" in

  let at_col_0 = p1_col.value ==:. 0 in
  let at_col_01 = at_col_0 |: (p1_col.value ==:. 1) in

  let shift_window_row win new_data =
    [ win.(2) <-- mux2 at_col_01 gnd win.(1).value
    ; win.(1) <-- mux2 at_col_0  gnd win.(0).value
    ; win.(0) <-- new_data
    ]
  in

  compile [
    p1_valid <--. 0;

    when_ (i.valid &: ~:is_newline &: ~:(done_reg.value)) [
      lb_rd_addr <-- in_col.value;
      p1_col <-- in_col.value;
      p1_row <-- in_row.value;
      p1_cell <-- is_at;
      p1_valid <--. 1;
      in_col <-- in_col.value +:. 1;
    ];

    when_ (i.valid &: is_newline &: ~:(done_reg.value)) [
      when_ (~:(width_known.value) &: (in_col.value >:. 0)) [
        padded_width <-- in_col.value;
        width_known <--. 1;
      ];
      in_row <-- in_row.value +:. 1;
      in_col <--. 0;
    ];

    when_ i.done_input [ done_reg <--. 1 ];

    when_ p1_valid.value (
      shift_window_row win_r2 lb2_masked @
      shift_window_row win_r1 lb1_masked @
      shift_window_row win_r0 p1_cell.value @
      [ lb_wr_addr <-- p1_col.value
      ; lb1_wr_en <--. 1
      ; lb1_wr_data <-- p1_cell.value
      ; lb2_wr_en <--. 1
      ; lb2_wr_data <-- lb1_masked
      ; p2_col <-- p1_col.value
      ; p2_row <-- p1_row.value
      ; p2_valid <--. 1
      ]
    );

    when_ (~:(p1_valid.value)) [ p2_valid <--. 0 ];

    when_ accessible [ count <-- count.value +:. 1 ];
  ];

  { O.count = count.value -- "count"; done_ = done_reg.value -- "done" }

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"paper_roll_core" create i
