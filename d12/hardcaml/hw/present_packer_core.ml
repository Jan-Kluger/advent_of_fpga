open Base
open Hardcaml
open Signal

module Config = struct
  let max_board_bits = 64
  let max_pieces = 32
  let max_placements_per_piece = 128
  let max_placements_total = 4096
  let max_steps = 100_000
  let piece_idx_bits = Int.ceil_log2 (max_pieces + 1)
  let placement_idx_bits = Int.ceil_log2 (max_placements_per_piece + 1)
  let placement_addr_bits = Int.ceil_log2 max_placements_total
  let step_counter_bits = Int.ceil_log2 (max_steps + 1)
end

module State = struct
  type t =
    | Idle
    | Load_piece_info
    | Load_placements
    | Run_filters
    | Dfs_init
    | Dfs_read_placement
    | Dfs_check_placement
    | Dfs_advance
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; board_area : 'a [@bits 16]
    ; required_area : 'a [@bits 16]
    ; total_pieces : 'a [@bits Config.piece_idx_bits]
    ; blocks_3x3 : 'a [@bits 16]
    ; piece_info_valid : 'a
    ; piece_num_placements : 'a [@bits Config.placement_idx_bits]
    ; placement_valid : 'a
    ; placement_mask : 'a [@bits Config.max_board_bits]
    ; result_ack : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; need_piece_info : 'a
    ; need_placement : 'a
    ; result_valid : 'a
    ; can_fit : 'a
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module State) reg_spec in
  let board_area = Always.Variable.reg reg_spec ~width:16 in
  let required_area = Always.Variable.reg reg_spec ~width:16 in
  let total_pieces = Always.Variable.reg reg_spec ~width:Config.piece_idx_bits in
  let blocks_3x3 = Always.Variable.reg reg_spec ~width:16 in
  let loading_piece_idx = Always.Variable.reg reg_spec ~width:Config.piece_idx_bits in
  let loading_placement_idx = Always.Variable.reg reg_spec ~width:Config.placement_idx_bits in
  let current_piece_num_placements =
    Always.Variable.reg reg_spec ~width:Config.placement_idx_bits
  in
  let write_addr = Always.Variable.reg reg_spec ~width:Config.placement_addr_bits in
  let piece_start_addrs =
    Array.init Config.max_pieces ~f:(fun _ ->
      Always.Variable.reg reg_spec ~width:Config.placement_addr_bits)
  in
  let piece_num_placements =
    Array.init Config.max_pieces ~f:(fun _ ->
      Always.Variable.reg reg_spec ~width:Config.placement_idx_bits)
  in
  let dfs_depth = Always.Variable.reg reg_spec ~width:Config.piece_idx_bits in
  let step_counter = Always.Variable.reg reg_spec ~width:Config.step_counter_bits in
  let stack_used =
    Array.init Config.max_pieces ~f:(fun _ ->
      Always.Variable.reg reg_spec ~width:Config.max_board_bits)
  in
  let stack_placement_idx =
    Array.init Config.max_pieces ~f:(fun _ ->
      Always.Variable.reg reg_spec ~width:Config.placement_idx_bits)
  in
  let result = Always.Variable.reg reg_spec ~width:1 in
  let result_valid_reg = Always.Variable.reg reg_spec ~width:1 in
  let ram_write_enable = Always.Variable.wire ~default:gnd in
  let ram_write_addr =
    Always.Variable.wire ~default:(zero Config.placement_addr_bits)
  in
  let ram_write_data = Always.Variable.wire ~default:(zero Config.max_board_bits) in
  let ram_read_addr = Always.Variable.reg reg_spec ~width:Config.placement_addr_bits in
  let placement_ram =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:Config.max_placements_total
      ~write_ports:
        [| { write_clock = i.clock
           ; write_enable = ram_write_enable.value
           ; write_address = ram_write_addr.value
           ; write_data = ram_write_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_enable = vdd; read_address = ram_read_addr.value }
        |]
      ()
  in
  let ram_read_data = placement_ram.(0) in
  let get_piece_start idx =
    mux idx (List.map (Array.to_list piece_start_addrs) ~f:(fun v -> v.value))
  in
  let get_piece_num_placements idx =
    mux idx (List.map (Array.to_list piece_num_placements) ~f:(fun v -> v.value))
  in
  let get_stack_used idx =
    mux idx (List.map (Array.to_list stack_used) ~f:(fun v -> v.value))
  in
  let get_stack_placement_idx idx =
    mux idx (List.map (Array.to_list stack_placement_idx) ~f:(fun v -> v.value))
  in
  let current_used = get_stack_used dfs_depth.value in
  let current_pl_idx = get_stack_placement_idx dfs_depth.value in
  let current_piece_start = get_piece_start dfs_depth.value in
  let current_piece_count = get_piece_num_placements dfs_depth.value in
  let placement_addr =
    current_piece_start +: uresize current_pl_idx Config.placement_addr_bits
  in
  let overlap = (current_used &: ram_read_data) <>:. 0 in
  let placement_exhausted = current_pl_idx >=: current_piece_count in
  let all_placed = dfs_depth.value >=: total_pieces.value in
  let step_limit = step_counter.value >=:. Config.max_steps in
  let set_piece_start idx value =
    List.mapi (Array.to_list piece_start_addrs) ~f:(fun i v ->
      Always.if_ (idx ==:. i) [ Always.(v <-- value) ] [])
    |> Always.proc
  in
  let set_piece_num_placements idx value =
    List.mapi (Array.to_list piece_num_placements) ~f:(fun i v ->
      Always.if_ (idx ==:. i) [ Always.(v <-- value) ] [])
    |> Always.proc
  in
  let set_stack_used idx value =
    List.mapi (Array.to_list stack_used) ~f:(fun i v ->
      Always.if_ (idx ==:. i) [ Always.(v <-- value) ] [])
    |> Always.proc
  in
  let set_stack_placement_idx idx value =
    List.mapi (Array.to_list stack_placement_idx) ~f:(fun i v ->
      Always.if_ (idx ==:. i) [ Always.(v <-- value) ] [])
    |> Always.proc
  in
  let inc_stack_placement_idx idx =
    List.mapi (Array.to_list stack_placement_idx) ~f:(fun i v ->
      Always.if_ (idx ==:. i) [ Always.(v <-- v.value +:. 1) ] [])
    |> Always.proc
  in
  Always.(
    compile
      [ sm.switch
          [ ( State.Idle
            , [ result_valid_reg <-- gnd
              ; when_
                  i.start
                  [ board_area <-- i.board_area
                  ; required_area <-- i.required_area
                  ; total_pieces <-- i.total_pieces
                  ; blocks_3x3 <-- i.blocks_3x3
                  ; loading_piece_idx <--. 0
                  ; write_addr <--. 0
                  ; sm.set_next Load_piece_info
                  ]
              ] )
          ; ( State.Load_piece_info
            , [ if_
                  (loading_piece_idx.value >=: total_pieces.value)
                  [ sm.set_next Run_filters ]
                  [ when_
                      i.piece_info_valid
                      [ set_piece_start loading_piece_idx.value write_addr.value
                      ; set_piece_num_placements
                          loading_piece_idx.value
                          i.piece_num_placements
                      ; current_piece_num_placements <-- i.piece_num_placements
                      ; loading_placement_idx <--. 0
                      ; sm.set_next Load_placements
                      ]
                  ]
              ] )
          ; ( State.Load_placements
            , [ if_
                  (loading_placement_idx.value >=: current_piece_num_placements.value)
                  [ loading_piece_idx <-- loading_piece_idx.value +:. 1
                  ; sm.set_next Load_piece_info
                  ]
                  [ when_
                      i.placement_valid
                      [ ram_write_enable <-- vdd
                      ; ram_write_addr <-- write_addr.value
                      ; ram_write_data <-- i.placement_mask
                      ; write_addr <-- write_addr.value +:. 1
                      ; loading_placement_idx <-- loading_placement_idx.value +:. 1
                      ]
                  ]
              ] )
          ; ( State.Run_filters
            , [ if_
                  (required_area.value >: board_area.value)
                  [ result <--. 0; result_valid_reg <-- vdd; sm.set_next Done ]
                  [ if_
                      (blocks_3x3.value >=: uresize total_pieces.value 16)
                      [ result <-- vdd; result_valid_reg <-- vdd; sm.set_next Done ]
                      [ sm.set_next Dfs_init ]
                  ]
              ] )
          ; ( State.Dfs_init
            , [ dfs_depth <--. 0
              ; step_counter <--. 0
              ; proc
                  (List.init Config.max_pieces ~f:(fun i ->
                     proc [ stack_used.(i) <--. 0; stack_placement_idx.(i) <--. 0 ]))
              ; sm.set_next Dfs_read_placement
              ] )
          ; ( State.Dfs_read_placement
            , [ step_counter <-- step_counter.value +:. 1
              ; if_
                  all_placed
                  [ result <-- vdd; result_valid_reg <-- vdd; sm.set_next Done ]
                  [ if_
                      step_limit
                      [ result <--. 0; result_valid_reg <-- vdd; sm.set_next Done ]
                      [ if_
                          placement_exhausted
                          [ if_
                              (dfs_depth.value ==:. 0)
                              [ result <--. 0; result_valid_reg <-- vdd; sm.set_next Done ]
                              [ dfs_depth <-- dfs_depth.value -:. 1
                              ; sm.set_next Dfs_read_placement
                              ]
                          ]
                          [ ram_read_addr <-- placement_addr
                          ; sm.set_next Dfs_check_placement
                          ]
                      ]
                  ]
              ] )
          ; ( State.Dfs_check_placement
            , [ sm.set_next Dfs_advance ] )
          ; ( State.Dfs_advance
            , [ if_
                  overlap
                  [ inc_stack_placement_idx dfs_depth.value
                  ; sm.set_next Dfs_read_placement
                  ]
                  [ inc_stack_placement_idx dfs_depth.value
                  ; set_stack_used (dfs_depth.value +:. 1) (current_used |: ram_read_data)
                  ; set_stack_placement_idx (dfs_depth.value +:. 1) (zero Config.placement_idx_bits)
                  ; dfs_depth <-- dfs_depth.value +:. 1
                  ; sm.set_next Dfs_read_placement
                  ]
              ] )
          ; (State.Done, [ when_ i.result_ack [ result_valid_reg <--. 0; sm.set_next Idle ] ])
          ]
      ]);
  { O.ready = sm.is State.Idle -- "ready"
  ; need_piece_info =
      (sm.is State.Load_piece_info &: (loading_piece_idx.value <: total_pieces.value))
      -- "need_piece_info"
  ; need_placement =
      (sm.is State.Load_placements
       &: (loading_placement_idx.value <: current_piece_num_placements.value))
      -- "need_placement"
  ; result_valid = result_valid_reg.value -- "result_valid"
  ; can_fit = result.value -- "can_fit"
  }
;;

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"present_packer_core" create i
;;
