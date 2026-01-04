open Hardcaml
open Signal

module Config = struct
  let max_cells = Grid_bram.Config.max_cells
  let addr_bits = Grid_bram.Config.addr_bits
  let row_bits = 8
  let col_bits = 8
  let entry_bits = row_bits + col_bits
  let ptr_bits = Bits.address_bits_for (max_cells + 1)
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; push : 'a
    ; push_row : 'a [@bits Config.row_bits]
    ; push_col : 'a [@bits Config.col_bits]
    ; pop : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { empty : 'a
    ; front_row : 'a [@bits Config.row_bits]
    ; front_col : 'a [@bits Config.col_bits]
    }
  [@@deriving hardcaml]
end

let create _scope (i : _ I.t) =
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  let head = Variable.reg reg_spec ~width:Config.ptr_bits in
  let tail = Variable.reg reg_spec ~width:Config.ptr_bits in

  let fifo_wr_addr = Variable.wire ~default:(zero Config.ptr_bits) in
  let fifo_wr_en = Variable.wire ~default:gnd in
  let fifo_wr_data = Variable.wire ~default:(zero Config.entry_bits) in
  let fifo_rd_addr = Variable.wire ~default:(zero Config.ptr_bits) in

  let fifo_ram =
    Ram.create ~collision_mode:Read_before_write ~size:Config.max_cells
      ~write_ports:
        [| { write_clock = i.clock
           ; write_address = sel_bottom fifo_wr_addr.value Config.addr_bits
           ; write_data = fifo_wr_data.value
           ; write_enable = fifo_wr_en.value
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock
           ; read_address = sel_bottom fifo_rd_addr.value Config.addr_bits
           ; read_enable = vdd
           }
        |]
      ()
  in

  compile
    [ fifo_rd_addr <-- head.value
    ; when_ i.push
        [ fifo_wr_addr <-- tail.value
        ; fifo_wr_en <--. 1
        ; fifo_wr_data <-- (i.push_row @: i.push_col)
        ; tail <-- tail.value +:. 1
        ]
    ; when_ i.pop [ head <-- head.value +:. 1 ]
    ];

  let empty = head.value ==: tail.value in
  let rd_data = fifo_ram.(0) in
  { O.empty
  ; front_row = select rd_data (Config.entry_bits - 1) Config.col_bits
  ; front_col = sel_bottom rd_data Config.col_bits
  }
