open Base
open Hardcaml
open Hardcaml_waveterm
open Hw
open Sim

let%expect_test "small fitting region" =
  let shapes =
    [ [ (0, 0); (0, 1); (1, 1); (1, 2); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 1); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 1); (0, 2); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 2); (2, 0); (2, 2) ]
    ; [ (0, 1); (0, 2); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 1); (1, 2); (2, 2) ]
    ]
  in
  let region = { Sim_core.width = 4; height = 4; requirements = [ 0; 0; 0; 0; 2; 0 ] } in
  let result = Sim_core.Reference.can_fit shapes region in
  Stdio.printf "4x4 with 2 shape-4s: %b\n" result;
  [%expect {| 4x4 with 2 shape-4s: true |}]
;;

let%expect_test "area filter rejects" =
  let shapes = [ [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (2, 0); (2, 1) ] ] in
  let region = { Sim_core.width = 3; height = 3; requirements = [ 2 ] } in
  let result = Sim_core.Reference.can_fit shapes region in
  Stdio.printf "3x3 with 2 7-cell shapes (14 > 9): %b\n" result;
  [%expect {| 3x3 with 2 7-cell shapes (14 > 9): false |}]
;;

let%expect_test "3x3 shortcut accepts" =
  let shapes = [ [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (2, 0); (2, 1) ] ] in
  let region = { Sim_core.width = 9; height = 9; requirements = [ 3 ] } in
  let result = Sim_core.Reference.can_fit shapes region in
  Stdio.printf "9x9 with 3 shapes (3x3 blocks = 9 >= 3): %b\n" result;
  [%expect {| 9x9 with 3 shapes (3x3 blocks = 9 >= 3): true |}]
;;

let%expect_test "hardware simulation basic" =
  let module I = Present_packer_core.I in
  let module O = Present_packer_core.O in
  let scope = Scope.create ~flatten_design:true () in
  let inputs = I.map2 I.port_names I.port_widths ~f:Signal.input in
  let outputs = Present_packer_core.create scope inputs in
  let circuit =
    Circuit.create_exn
      ~name:"test_sim"
      (O.to_list (O.map2 O.port_names outputs ~f:Signal.output))
  in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let waves, sim = Waveform.create sim in
  let i : Bits.t ref I.t = I.map I.port_names ~f:(Cyclesim.in_port sim) in
  let o : Bits.t ref O.t = O.map O.port_names ~f:(Cyclesim.out_port sim) in
  let open Bits in
  i.I.clear := vdd;
  Cyclesim.cycle sim;
  i.I.clear := gnd;
  i.I.start := vdd;
  i.I.board_area := of_int ~width:16 36;
  i.I.required_area := of_int ~width:16 14;
  i.I.total_pieces := of_int ~width:Present_packer_core.Config.piece_idx_bits 2;
  i.I.blocks_3x3 := of_int ~width:16 4;
  Cyclesim.cycle sim;
  i.I.start := gnd;
  for _ = 0 to 100 do
    if to_bool !(o.O.need_piece_info)
    then (
      i.I.piece_info_valid := vdd;
      i.I.piece_num_placements := of_int ~width:Present_packer_core.Config.placement_idx_bits 4;
      Cyclesim.cycle sim;
      i.I.piece_info_valid := gnd;
      for mask_idx = 0 to 3 do
        while not (to_bool !(o.O.need_placement)) do
          Cyclesim.cycle sim
        done;
        i.I.placement_valid := vdd;
        i.I.placement_mask := of_int ~width:Present_packer_core.Config.max_board_bits (1 lsl mask_idx);
        Cyclesim.cycle sim;
        i.I.placement_valid := gnd
      done)
    else Cyclesim.cycle sim;
    if to_bool !(o.O.result_valid) then ()
  done;
  Stdio.printf "Result valid: %b, Can fit: %b\n" (to_bool !(o.O.result_valid)) (to_bool !(o.O.can_fit));
  Waveform.expect ~serialize_to:"basic_hw_test" ~display_height:40 ~display_width:120 waves;
  [%expect {|
    Result valid: true, Can fit: true
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clear             ││────────┐                                                                                         │
    │                  ││        └─────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬─────────────────────────────────────────────────────────────────────────────────────────│
    │blocks_3x3        ││ 0000   │0004                                                                                     │
    │                  ││────────┴─────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬─────────────────────────────────────────────────────────────────────────────────────────│
    │board_area        ││ 0000   │0024                                                                                     │
    │                  ││────────┴─────────────────────────────────────────────────────────────────────────────────────────│
    │piece_info_valid  ││                ┌───────┐                                       ┌───────┐                         │
    │                  ││────────────────┘       └───────────────────────────────────────┘       └─────────────────────────│
    │                  ││────────────────┬─────────────────────────────────────────────────────────────────────────────────│
    │piece_num_placemen││ 00             │04                                                                               │
    │                  ││────────────────┴─────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬───────┬───────┬───────┬───────────────────────┬───────┬───────┬───────┬─│
    │placement_mask    ││ 0000000000000000       │000000.│000000.│000000.│0000000000000008       │000000.│000000.│000000.│0│
    │                  ││────────────────────────┴───────┴───────┴───────┴───────────────────────┴───────┴───────┴───────┴─│
    │placement_valid   ││                        ┌───────────────────────────────┐               ┌─────────────────────────│
    │                  ││────────────────────────┘                               └───────────────┘                         │
    │                  ││────────┬─────────────────────────────────────────────────────────────────────────────────────────│
    │required_area     ││ 0000   │000E                                                                                     │
    │                  ││────────┴─────────────────────────────────────────────────────────────────────────────────────────│
    │result_ack        ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │start             ││        ┌───────┐                                                                                 │
    │                  ││────────┘       └─────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬─────────────────────────────────────────────────────────────────────────────────────────│
    │total_pieces      ││ 00     │02                                                                                       │
    │                  ││────────┴─────────────────────────────────────────────────────────────────────────────────────────│
    │can_fit           ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │need_piece_info   ││                ┌───────┐                                       ┌───────┐                         │
    │                  ││────────────────┘       └───────────────────────────────────────┘       └─────────────────────────│
    │need_placement    ││                        ┌───────────────────────────────┐               ┌─────────────────────────│
    │                  ││────────────────────────┘                               └───────────────┘                         │
    │ready             ││────────────────┐                                                                                 │
    │                  ││                └─────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ac7edaa60d29bc09937fbc5a1ec68813
    |}]
;;

let%expect_test "hardware DFS shape4 on 4x4 board" =
  let shapes =
    [ [ (0, 0); (0, 1); (1, 1); (1, 2); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 1); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 1); (0, 2); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 2); (2, 0); (2, 2) ]
    ; [ (0, 1); (0, 2); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2) ]
    ; [ (0, 0); (0, 1); (0, 2); (1, 1); (1, 2); (2, 2) ]
    ]
  in
  let region = { Sim_core.width = 4; height = 4; requirements = [ 0; 0; 0; 0; 2; 0 ] } in
  Stdio.printf "Reference: %b\n" (Sim_core.Reference.can_fit shapes region);
  let sim = Sim_core.Sim.create_sim () in
  let prepared = Sim_core.prepare_region shapes region in
  Stdio.printf "Board: %dx%d, pieces=%d, placements per piece=%d\n"
    region.width region.height prepared.total_pieces 
    (List.length (List.hd_exn prepared.pieces));
  let result = Sim_core.Sim.run_region ~debug:false sim prepared in
  Stdio.printf "Hardware: %b\n" result;
  [%expect {|
    Reference: true
    Board: 4x4, pieces=2, placements per piece=32
    Hardware: true
    |}]
;;

let%expect_test "hardware DFS trivial single cells" =
  let shapes = [ [ (0, 0) ] ] in
  let region = { Sim_core.width = 2; height = 2; requirements = [ 2 ] } in
  Stdio.printf "Reference: %b\n" (Sim_core.Reference.can_fit shapes region);
  let sim = Sim_core.Sim.create_sim () in
  let prepared = Sim_core.prepare_region shapes region in
  let result = Sim_core.Sim.run_region ~debug:false sim prepared in
  Stdio.printf "Hardware: %b\n" result;
  [%expect {|
    Reference: true
    Hardware: true
    |}]
;;
