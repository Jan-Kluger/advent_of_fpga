open Base
open Hardcaml
open Hw

type cell = int * int
type shape = cell list

type region =
  { width : int
  ; height : int
  ; requirements : int list
  }

type prepared_region =
  { board_area : int
  ; required_area : int
  ; total_pieces : int
  ; blocks_3x3 : int
  ; pieces : int list list
  }

let trim s = String.concat (String.split s ~on:'\r') |> String.strip

let parse_cells lines =
  List.concat_mapi lines ~f:(fun r line ->
    String.to_list line
    |> List.filter_mapi ~f:(fun c ch ->
      if Char.equal ch '#' then Some (r, c) else None))
;;

let parse_input ic =
  let content = In_channel.input_all ic in
  let raw_lines = String.split_lines content in
  let lines = List.map raw_lines ~f:trim in
  let shapes = Hashtbl.create (module Int) in
  let rec flush cur =
    match cur with
    | Some (idx, ls) -> Hashtbl.set shapes ~key:idx ~data:(parse_cells (List.rev ls))
    | None -> ()
  and go regions cur = function
    | [] ->
      flush cur;
      regions
    | "" :: rest ->
      flush cur;
      go regions None rest
    | line :: rest ->
      (match String.lsplit2 line ~on:':' with
       | Some (dims, counts) when String.is_substring dims ~substring:"x" ->
         flush cur;
         let w, h =
           match String.split dims ~on:'x' with
           | [ ws; hs ] -> Int.of_string ws, Int.of_string hs
           | _ -> failwith "bad dims"
         in
         let parts =
           String.split (trim counts) ~on:' '
           |> List.filter ~f:(fun s -> not (String.is_empty s))
         in
         let reqs = List.map parts ~f:Int.of_string in
         let region = { width = w; height = h; requirements = reqs } in
         go (region :: regions) None rest
       | Some (idx, "") ->
         flush cur;
         go regions (Some (Int.of_string idx, [])) rest
       | _ ->
         let cur' =
           match cur with
           | Some (idx, ls) -> Some (idx, line :: ls)
           | None -> None
         in
         go regions cur' rest)
  in
  let regions = List.rev (go [] None lines) in
  let max_idx =
    Hashtbl.fold shapes ~init:(-1) ~f:(fun ~key ~data:_ acc -> Int.max key acc)
  in
  let shapes_list =
    List.init (max_idx + 1) ~f:(fun i ->
      match Hashtbl.find shapes i with
      | Some cells -> cells
      | None -> [])
  in
  shapes_list, regions
;;

let normalize cells =
  match cells with
  | [] -> []
  | _ ->
    let min_r =
      List.fold cells ~init:Int.max_value ~f:(fun acc (r, _) -> Int.min acc r)
    in
    let min_c =
      List.fold cells ~init:Int.max_value ~f:(fun acc (_, c) -> Int.min acc c)
    in
    let shifted = List.map cells ~f:(fun (r, c) -> r - min_r, c - min_c) in
    List.sort shifted ~compare:[%compare: int * int]
;;

let rotate cells =
  let rotated = List.map cells ~f:(fun (r, c) -> c, -r) in
  normalize rotated
;;

let flip cells =
  let flipped = List.map cells ~f:(fun (r, c) -> r, -c) in
  normalize flipped
;;

let all_orientations cells =
  let r0 = normalize cells in
  let r1 = rotate r0 in
  let r2 = rotate r1 in
  let r3 = rotate r2 in
  let f0 = flip r0 in
  let f1 = flip r1 in
  let f2 = flip r2 in
  let f3 = flip r3 in
  List.dedup_and_sort [ r0; r1; r2; r3; f0; f1; f2; f3 ] ~compare:[%compare: (int * int) list]
;;

let bounding_height cells =
  1 + List.fold cells ~init:0 ~f:(fun acc (r, _) -> Int.max acc r)
;;

let bounding_width cells =
  1 + List.fold cells ~init:0 ~f:(fun acc (_, c) -> Int.max acc c)
;;

let to_mask width cells =
  List.fold cells ~init:0 ~f:(fun m (r, c) -> m lor (1 lsl ((r * width) + c)))
;;

let placements ~width ~height cells =
  let place_orientation orient =
    let bh = bounding_height orient in
    let bw = bounding_width orient in
    let max_dr = Int.max 0 (height - bh + 1) in
    let max_dc = Int.max 0 (width - bw + 1) in
    List.concat_map (List.init max_dr ~f:Fn.id) ~f:(fun dr ->
      List.map (List.init max_dc ~f:Fn.id) ~f:(fun dc ->
        let shifted = List.map orient ~f:(fun (r, c) -> r + dr, c + dc) in
        to_mask width shifted))
  in
  List.concat_map (all_orientations cells) ~f:place_orientation
  |> List.dedup_and_sort ~compare:Int.compare
;;

let prepare_region shapes region =
  let { width; height; requirements } = region in
  let shape_area s = List.length s in
  let board_area = width * height in
  let required_area =
    List.fold2_exn shapes requirements ~init:0 ~f:(fun acc s c ->
      acc + (shape_area s * c))
  in
  let total_pieces = List.fold requirements ~init:0 ~f:( + ) in
  let blocks_3x3 = (width / 3) * (height / 3) in
  let make_pieces shape_idx count =
    if count = 0
    then []
    else (
      let shape = List.nth_exn shapes shape_idx in
      let masks = placements ~width ~height shape in
      List.init count ~f:(fun _ -> masks))
  in
  let pieces = List.concat (List.mapi requirements ~f:make_pieces) in
  { board_area; required_area; total_pieces; blocks_3x3; pieces }
;;

module Reference = struct
  type piece =
    { id : int
    ; masks : int list
    }

  let search pieces =
    let rec go used = function
      | [] -> true
      | _ as pieces ->
        let count_valid p =
          let valid = List.filter p.masks ~f:(fun m -> used land m = 0) in
          List.length valid, p.id, valid
        in
        let ranked =
          List.sort (List.map pieces ~f:count_valid) ~compare:[%compare: int * int * _]
        in
        (match ranked with
         | (0, _, _) :: _ -> false
         | (_, id, valid) :: _ ->
           let others = List.filter pieces ~f:(fun p -> p.id <> id) in
           List.exists valid ~f:(fun m -> go (used lor m) others)
         | [] -> true)
    in
    go 0 pieces
  ;;

  let can_fit shapes region =
    let { width; height; requirements } = region in
    let shape_area s = List.length s in
    let required_area =
      List.fold2_exn shapes requirements ~init:0 ~f:(fun acc s c ->
        acc + (shape_area s * c))
    in
    let total_count = List.fold requirements ~init:0 ~f:( + ) in
    let board_area = width * height in
    let blocks_3x3 = (width / 3) * (height / 3) in
    if required_area > board_area
    then false
    else if blocks_3x3 >= total_count
    then true
    else (
      let make_pieces si count =
        let shape = List.nth_exn shapes si in
        let masks = placements ~width ~height shape in
        List.init count ~f:(fun i -> { id = (si * 1000) + i; masks })
      in
      let pieces = List.concat (List.mapi requirements ~f:make_pieces) in
      search pieces)
  ;;

  let solve shapes regions = List.count regions ~f:(can_fit shapes)
end

module Sim = struct
  module I = Present_packer_core.I
  module O = Present_packer_core.O

  let create_sim () =
    let scope = Scope.create ~flatten_design:true () in
    let inputs = I.map2 I.port_names I.port_widths ~f:Signal.input in
    let outputs = Present_packer_core.create scope inputs in
    let circuit =
      Circuit.create_exn
        ~name:"present_packer_sim"
        (O.to_list (O.map2 O.port_names outputs ~f:Signal.output))
    in
    let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
    let i : Bits.t ref I.t = I.map I.port_names ~f:(fun name -> Cyclesim.in_port sim name) in
    let o : Bits.t ref O.t = O.map O.port_names ~f:(fun name -> Cyclesim.out_port sim name) in
    sim, i, o
  ;;

  let run_region ?(debug = false) (sim, (i : Bits.t ref I.t), (o : Bits.t ref O.t)) prepared =
    let open Bits in
    i.I.clear := vdd;
    Cyclesim.cycle sim;
    i.I.clear := gnd;
    Cyclesim.cycle sim;
    let board_too_large = prepared.board_area > Present_packer_core.Config.max_board_bits in
    let too_many_pieces = prepared.total_pieces > Present_packer_core.Config.max_pieces in
    let skip_dfs = board_too_large || too_many_pieces in
    if debug then Stdio.printf "DEBUG: skip_dfs=%b, pieces=%d\n%!" skip_dfs prepared.total_pieces;
    i.I.start := vdd;
    i.I.board_area := of_int ~width:16 prepared.board_area;
    i.I.required_area := of_int ~width:16 prepared.required_area;
    i.I.total_pieces :=
      of_int
        ~width:Present_packer_core.Config.piece_idx_bits
        (if skip_dfs then 0 else prepared.total_pieces);
    i.I.blocks_3x3 := of_int ~width:16 prepared.blocks_3x3;
    Cyclesim.cycle sim;
    i.I.start := gnd;
    if not skip_dfs
    then (
      if debug then Stdio.printf "DEBUG: loading %d pieces\n%!" (List.length prepared.pieces);
      List.iteri prepared.pieces ~f:(fun piece_idx piece_masks ->
        if debug then Stdio.printf "DEBUG: waiting for piece %d info request\n%!" piece_idx;
        while not (to_bool !(o.O.need_piece_info)) do
          Cyclesim.cycle sim
        done;
        let num_pl = Int.min (List.length piece_masks) (Present_packer_core.Config.max_placements_per_piece - 1) in
        if debug then Stdio.printf "DEBUG: piece %d has %d placements\n%!" piece_idx num_pl;
        i.I.piece_info_valid := vdd;
        i.I.piece_num_placements := of_int ~width:Present_packer_core.Config.placement_idx_bits num_pl;
        Cyclesim.cycle sim;
        i.I.piece_info_valid := gnd;
        let masks_to_load = List.take piece_masks (Present_packer_core.Config.max_placements_per_piece - 1) in
        List.iteri masks_to_load ~f:(fun mask_idx mask ->
          while not (to_bool !(o.O.need_placement)) do
            Cyclesim.cycle sim
          done;
          if debug && mask_idx < 3 then Stdio.printf "DEBUG:   placement %d: mask=0x%x\n%!" mask_idx mask;
          i.I.placement_valid := vdd;
          i.I.placement_mask := of_int ~width:Present_packer_core.Config.max_board_bits mask;
          Cyclesim.cycle sim;
          i.I.placement_valid := gnd)));
    if debug then Stdio.printf "DEBUG: waiting for result\n%!";
    let cycles = ref 0 in
    while not (to_bool !(o.O.result_valid)) do
      Cyclesim.cycle sim;
      Int.incr cycles
    done;
    if debug then Stdio.printf "DEBUG: got result after %d cycles\n%!" !cycles;
    let result = to_bool !(o.O.can_fit) in
    i.I.result_ack := vdd;
    Cyclesim.cycle sim;
    i.I.result_ack := gnd;
    result
  ;;

  let solve shapes regions =
    let sim = create_sim () in
    List.count regions ~f:(fun region ->
      let prepared = prepare_region shapes region in
      run_region sim prepared)
  ;;
end
