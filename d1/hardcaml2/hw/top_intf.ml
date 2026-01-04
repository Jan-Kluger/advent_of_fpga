module I = struct
  type 'a t = {
    clock : 'a; [@bits 1] [@rtlname "clk"]
    clear : 'a; [@bits 1] [@rtlname "rst"]
    dir : 'a; [@bits 1] [@rtlname "dir_in"]
    num : 'a; [@bits 16] [@rtlname "num_in"]
    valid : 'a; [@bits 1] [@rtlname "valid_in"]
    eof : 'a; [@bits 1] [@rtlname "eof_in"]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    count : 'a; [@bits 16] [@rtlname "count_out"]
    done_ : 'a; [@bits 1] [@rtlname "done_out"]
    rotation : 'a; [@bits 7] [@rtlname "rotation_out"]
  }
  [@@deriving hardcaml]
end
