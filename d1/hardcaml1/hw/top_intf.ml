module I = struct
  type 'a t = {
    clock : 'a; [@bits 1] [@rtlname "clk"]
    clear : 'a; [@bits 1] [@rtlname "rst"]
    data : 'a; [@bits 8] [@rtlname "data_in"]
    valid : 'a; [@bits 1] [@rtlname "valid_in"]
    eof : 'a; [@bits 1] [@rtlname "eof_in"]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    count : 'a; [@bits 16] [@rtlname "count_out"]
    done_ : 'a; [@bits 1] [@rtlname "done_out"]
    rotation : 'a; [@bits 16] [@rtlname "rotation_out"]
  }
  [@@deriving hardcaml]
end
