open Hardcaml
open Signal

module Config = struct
  let addr_bits = Grid_bram.Config.addr_bits
  let degree_bits = Grid_bram.Config.degree_bits
  let col_bits = Init_stream.Config.col_bits
  let row_bits = Init_stream.Config.row_bits
end

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
    { count_part1 : 'a [@bits 32]
    ; count_part2 : 'a [@bits 32]
    ; all_done : 'a
    }
  [@@deriving hardcaml]
end

module Phase = struct
  type t =
    | Init
    | Peel
    | Done
  [@@deriving sexp_of, enumerate, compare]
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module Phase) reg_spec in

  let init =
    Init_stream.create
      (Scope.sub_scope scope "init")
      { Init_stream.I.clock = i.clock
      ; clear = i.clear
      ; byte_in = i.byte_in
      ; valid = i.valid
      ; done_input = i.done_input
      }
  in

  let grid_width = Variable.reg reg_spec ~width:Config.col_bits in
  let grid_height = Variable.reg reg_spec ~width:Config.row_bits in

  let peel_start = Variable.wire ~default:gnd in
  let fifo_push = Variable.wire ~default:gnd in
  let fifo_push_row = Variable.wire ~default:(zero Config.row_bits) in
  let fifo_push_col = Variable.wire ~default:(zero Config.col_bits) in
  let fifo_pop = Variable.wire ~default:gnd in
  let bram_wr_en = Variable.wire ~default:gnd in
  let bram_wr_addr = Variable.wire ~default:(zero Config.addr_bits) in
  let bram_wr_alive = Variable.wire ~default:gnd in
  let bram_wr_degree = Variable.wire ~default:(zero Config.degree_bits) in
  let bram_rd_addr = Variable.wire ~default:(zero Config.addr_bits) in

  let fifo =
    Fifo_queue.create
      (Scope.sub_scope scope "fifo")
      { Fifo_queue.I.clock = i.clock
      ; clear = i.clear
      ; push = fifo_push.value
      ; push_row = fifo_push_row.value
      ; push_col = fifo_push_col.value
      ; pop = fifo_pop.value
      }
  in

  let bram =
    Grid_bram.create
      (Scope.sub_scope scope "bram")
      { Grid_bram.I.clock = i.clock
      ; wr_en = bram_wr_en.value
      ; wr_addr = bram_wr_addr.value
      ; wr_alive = bram_wr_alive.value
      ; wr_degree = bram_wr_degree.value
      ; rd_addr = bram_rd_addr.value
      }
  in

  let peel =
    Peel_fsm.create
      (Scope.sub_scope scope "peel")
      { Peel_fsm.I.clock = i.clock
      ; clear = i.clear
      ; start = peel_start.value
      ; grid_width = grid_width.value
      ; grid_height = grid_height.value
      ; fifo_empty = fifo.empty
      ; fifo_front_row = fifo.front_row
      ; fifo_front_col = fifo.front_col
      ; bram_rd_alive = bram.rd_alive
      ; bram_rd_degree = bram.rd_degree
      }
  in

  compile
    [ sm.switch
        [ ( Init
          , [ fifo_push <-- init.fifo_push
            ; fifo_push_row <-- init.fifo_push_row
            ; fifo_push_col <-- init.fifo_push_col
            ; bram_wr_en <-- init.bram_wr_en
            ; bram_wr_addr <-- init.bram_wr_addr
            ; bram_wr_alive <-- init.bram_wr_alive
            ; bram_wr_degree <-- init.bram_wr_degree
            ; when_ init.init_done
                [ grid_width <-- init.grid_width
                ; grid_height <-- init.grid_height
                ; peel_start <--. 1
                ; sm.set_next Peel
                ]
            ] )
        ; ( Peel
          , [ fifo_push <-- peel.fifo_push
            ; fifo_push_row <-- peel.fifo_push_row
            ; fifo_push_col <-- peel.fifo_push_col
            ; fifo_pop <-- peel.fifo_pop
            ; bram_wr_en <-- peel.bram_wr_en
            ; bram_wr_addr <-- peel.bram_wr_addr
            ; bram_wr_alive <-- peel.bram_wr_alive
            ; bram_wr_degree <-- peel.bram_wr_degree
            ; bram_rd_addr <-- peel.bram_rd_addr
            ; when_ peel.done_ [ sm.set_next Done ]
            ] )
        ; (Done, [])
        ]
    ];

  let _ = sm.current -- "phase" in

  { O.count_part1 = init.count_part1 -- "count_part1"
  ; count_part2 = peel.removed_count -- "count_part2"
  ; all_done = sm.is Done -- "all_done"
  }

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"paper_roll_part2" create i
