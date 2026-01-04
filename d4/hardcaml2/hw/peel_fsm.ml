open Hardcaml
open Signal
open Always

module Config = struct
  let addr_bits = Grid_bram.Config.addr_bits
  let degree_bits = Grid_bram.Config.degree_bits
  let col_bits = 8
  let row_bits = 8
end

module State = struct
  type t =
    | Idle
    | Check_fifo
    | Wait_fifo_read
    | Read_alive
    | Check_alive
    | Mark_dead
    | Calc_neighbor
    | Check_bounds
    | Read_neighbor
    | Update_neighbor
    | Next_neighbor
    | Done
  [@@deriving sexp_of, enumerate, compare]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; grid_width : 'a [@bits Config.col_bits]
    ; grid_height : 'a [@bits Config.row_bits]
    ; fifo_empty : 'a
    ; fifo_front_row : 'a [@bits Config.row_bits]
    ; fifo_front_col : 'a [@bits Config.col_bits]
    ; bram_rd_alive : 'a
    ; bram_rd_degree : 'a [@bits Config.degree_bits]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { fifo_pop : 'a
    ; fifo_push : 'a
    ; fifo_push_row : 'a [@bits Config.row_bits]
    ; fifo_push_col : 'a [@bits Config.col_bits]
    ; bram_rd_addr : 'a [@bits Config.addr_bits]
    ; bram_wr_en : 'a
    ; bram_wr_addr : 'a [@bits Config.addr_bits]
    ; bram_wr_alive : 'a
    ; bram_wr_degree : 'a [@bits Config.degree_bits]
    ; removed_count : 'a [@bits 32]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

let neighbor_row_offsets = [| -1; -1; -1; 0; 0; 1; 1; 1 |]
let neighbor_col_offsets = [| -1; 0; 1; -1; 1; -1; 0; 1 |]

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) reg_spec in

  let current_row = Variable.reg reg_spec ~width:Config.row_bits in
  let current_col = Variable.reg reg_spec ~width:Config.col_bits in
  let current_addr = Variable.reg reg_spec ~width:Config.addr_bits in
  let neighbor_idx = Variable.reg reg_spec ~width:4 in
  let neighbor_row = Variable.reg reg_spec ~width:(Config.row_bits + 1) in
  let neighbor_col = Variable.reg reg_spec ~width:(Config.col_bits + 1) in
  let neighbor_addr = Variable.reg reg_spec ~width:Config.addr_bits in
  let removed_count = Variable.reg reg_spec ~width:32 in

  let fifo_pop = Variable.wire ~default:gnd in
  let fifo_push = Variable.wire ~default:gnd in
  let fifo_push_row = Variable.wire ~default:(zero Config.row_bits) in
  let fifo_push_col = Variable.wire ~default:(zero Config.col_bits) in
  let bram_rd_addr = Variable.wire ~default:(zero Config.addr_bits) in
  let bram_wr_en = Variable.wire ~default:gnd in
  let bram_wr_addr = Variable.wire ~default:(zero Config.addr_bits) in
  let bram_wr_alive = Variable.wire ~default:gnd in
  let bram_wr_degree = Variable.wire ~default:(zero Config.degree_bits) in

  let width_ext = uresize i.grid_width Config.addr_bits in

  let calc_addr row col =
    let row_offset = uresize row Config.addr_bits *: width_ext in
    sel_bottom row_offset Config.addr_bits +: uresize col Config.addr_bits
  in

  let dr_rom =
    Array.map (fun d -> of_int ~width:(Config.row_bits + 1) d) neighbor_row_offsets
  in
  let dc_rom =
    Array.map (fun d -> of_int ~width:(Config.col_bits + 1) d) neighbor_col_offsets
  in
  let dr = mux (sel_bottom neighbor_idx.value 3) (Array.to_list dr_rom) in
  let dc = mux (sel_bottom neighbor_idx.value 3) (Array.to_list dc_rom) in

  let row_in_bounds =
    ~:(msb neighbor_row.value)
    &: (sel_bottom neighbor_row.value Config.row_bits <: i.grid_height)
  in
  let col_in_bounds =
    ~:(msb neighbor_col.value)
    &: (sel_bottom neighbor_col.value Config.col_bits <: i.grid_width)
  in
  let nrow = sel_bottom neighbor_row.value Config.row_bits in
  let ncol = sel_bottom neighbor_col.value Config.col_bits in
  let new_degree = i.bram_rd_degree -:. 1 in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ i.start [ sm.set_next Check_fifo ] ] )
        ; ( Check_fifo
          , [ if_ i.fifo_empty
                [ sm.set_next Done ]
                [ fifo_pop <--. 1; sm.set_next Wait_fifo_read ]
            ] )
        ; ( Wait_fifo_read
          , [ current_row <-- i.fifo_front_row
            ; current_col <-- i.fifo_front_col
            ; current_addr <-- calc_addr i.fifo_front_row i.fifo_front_col
            ; sm.set_next Read_alive
            ] )
        ; ( Read_alive
          , [ bram_rd_addr <-- current_addr.value; sm.set_next Check_alive ] )
        ; ( Check_alive
          , [ if_ i.bram_rd_alive
                [ sm.set_next Mark_dead ]
                [ sm.set_next Check_fifo ]
            ] )
        ; ( Mark_dead
          , [ bram_wr_en <--. 1
            ; bram_wr_addr <-- current_addr.value
            ; bram_wr_alive <--. 0
            ; bram_wr_degree <-- i.bram_rd_degree
            ; removed_count <-- removed_count.value +:. 1
            ; neighbor_idx <--. 0
            ; sm.set_next Calc_neighbor
            ] )
        ; ( Calc_neighbor
          , [ neighbor_row
              <-- uresize current_row.value (Config.row_bits + 1)
                  +: sresize dr (Config.row_bits + 1)
            ; neighbor_col
              <-- uresize current_col.value (Config.col_bits + 1)
                  +: sresize dc (Config.col_bits + 1)
            ; sm.set_next Check_bounds
            ] )
        ; ( Check_bounds
          , [ if_ (row_in_bounds &: col_in_bounds)
                [ neighbor_addr <-- calc_addr nrow ncol; sm.set_next Read_neighbor ]
                [ sm.set_next Next_neighbor ]
            ] )
        ; ( Read_neighbor
          , [ bram_rd_addr <-- neighbor_addr.value; sm.set_next Update_neighbor ] )
        ; ( Update_neighbor
          , [ when_ i.bram_rd_alive
                [ bram_wr_en <--. 1
                ; bram_wr_addr <-- neighbor_addr.value
                ; bram_wr_alive <--. 1
                ; bram_wr_degree <-- new_degree
                ; when_ (i.bram_rd_degree ==:. 4)
                    [ fifo_push <--. 1
                    ; fifo_push_row <-- nrow
                    ; fifo_push_col <-- ncol
                    ]
                ]
            ; sm.set_next Next_neighbor
            ] )
        ; ( Next_neighbor
          , [ if_ (neighbor_idx.value ==:. 7)
                [ sm.set_next Check_fifo ]
                [ neighbor_idx <-- neighbor_idx.value +:. 1; sm.set_next Calc_neighbor ]
            ] )
        ; ( Done, [] )
        ]
    ];

  let _ = sm.current -- "peel_state" in

  { O.fifo_pop = fifo_pop.value
  ; fifo_push = fifo_push.value
  ; fifo_push_row = fifo_push_row.value
  ; fifo_push_col = fifo_push_col.value
  ; bram_rd_addr = bram_rd_addr.value
  ; bram_wr_en = bram_wr_en.value
  ; bram_wr_addr = bram_wr_addr.value
  ; bram_wr_alive = bram_wr_alive.value
  ; bram_wr_degree = bram_wr_degree.value
  ; removed_count = removed_count.value -- "removed_count"
  ; done_ = sm.is Done -- "peel_done"
  }
