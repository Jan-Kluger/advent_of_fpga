open Hardcaml
open Signal

module Config = struct
  let max_width = 160
  let max_height = 160
  let max_cells = max_width * max_height
  let addr_bits = Bits.address_bits_for max_cells
  let degree_bits = 4
end

module I = struct
  type 'a t =
    { clock : 'a
    ; wr_en : 'a
    ; wr_addr : 'a [@bits Config.addr_bits]
    ; wr_alive : 'a
    ; wr_degree : 'a [@bits Config.degree_bits]
    ; rd_addr : 'a [@bits Config.addr_bits]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { rd_alive : 'a
    ; rd_degree : 'a [@bits Config.degree_bits]
    }
  [@@deriving hardcaml]
end

let create _scope (i : _ I.t) =
  let wr_data = i.wr_alive @: i.wr_degree in
  let ram_out =
    Ram.create ~collision_mode:Read_before_write ~size:Config.max_cells
      ~write_ports:
        [| { write_clock = i.clock
           ; write_address = i.wr_addr
           ; write_data = wr_data
           ; write_enable = i.wr_en
           }
        |]
      ~read_ports:
        [| { read_clock = i.clock; read_address = i.rd_addr; read_enable = vdd } |]
      ()
  in
  let rd_data = ram_out.(0) in
  { O.rd_alive = bit rd_data Config.degree_bits
  ; rd_degree = sel_bottom rd_data Config.degree_bits
  }

