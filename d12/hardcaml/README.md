Present Packer - Hardcaml FPGA Implementation

This project solves the Advent of Code Day 12 "Christmas Tree Farm" puzzle using
Hardcaml, a hardware description library for OCaml. The solution can be simulated
locally and synthesized to Verilog for FPGA deployment.

PROBLEM OVERVIEW

Given a set of tetromino-like shapes and rectangular regions, determine how many
regions can fit all their required shapes without overlap. Shapes can be rotated
and flipped.

ALGORITHM

The solver uses a two-phase approach:

1. Early Filters (computed by host, checked by hardware)

   - Area filter: reject if sum of shape areas exceeds board area
   - 3x3 shortcut: accept if (width/3)\*(height/3) >= total_pieces
     Since all shapes fit in 3x3 bounding boxes, having enough non-overlapping
     3x3 blocks guarantees a solution exists

2. Bounded DFS Search (hardware, for small boards only)
   - Explicit stack-based depth-first search implemented as FSM
   - Pieces placed one at a time, trying each valid placement
   - Backtrack when a placement fails (overlaps with used cells)
   - Step counter limits search to prevent infinite loops
   - Bitmask representation for fast overlap detection using bitwise AND
   - BRAM stores placement masks, registers store DFS stack state

For this puzzle input, ALL 1000 regions are resolved by filters alone:

- 413 rejected by area filter
- 587 accepted by 3x3 shortcut
- 0 require DFS

The hardware supports DFS for smaller boards (up to 64 bits, e.g., 8x8) but the
puzzle input only contains larger regions that are resolved by filters.

HOST vs FPGA SPLIT

Host (OCaml) handles:

- Parsing input file
- Computing all rotations and flips of shapes
- Generating placement bitmasks for each position
- Pre-computing filter values (board_area, required_area, blocks_3x3)
- Complex computations like division are done on host to avoid hardware complexity

FPGA handles:

- Storing placement masks in BRAM (for DFS cases)
- Running early filter comparisons (combinational)
- Executing bounded DFS with hardware stack (when needed)
- Returning can_fit result

HARDWARE DESIGN

Config Parameters:

- max_board_bits = 64 (supports up to 8x8 boards for DFS)
- max_pieces = 32
- max_placements_per_piece = 128
- max_placements_total = 4096
- max_steps = 100,000

State Machine:

- Idle: wait for start signal
- Load_piece_info: receive piece metadata (start address, num placements)
- Load_placements: stream placement masks into BRAM
- Run_filters: check area and 3x3 filters combinatorially
- Dfs_init: initialize stack (depth=0, all stack arrays to zero)
- Dfs_read_placement: compute RAM address, fetch placement mask
- Dfs_check_placement: wait one cycle for BRAM read latency
- Dfs_advance: test overlap, advance depth or backtrack
- Done: output result, wait for ack

Memory Layout:

- BRAM: placement_ram[4096] stores all placement bitmasks
- Registers: piece_start_addrs[32], piece_num_placements[32]
- Registers: stack_used[32], stack_placement_idx[32] for DFS stack
- Registers: dfs_depth, step_counter

Key Design Decisions:

- No division in hardware. Host pre-computes all quotients (blocks_3x3)
- RAM read address is a register, not a wire, to hold value across states
- Single-cycle overlap test using bitwise AND: (used_mask & placement_mask) != 0
- Bounded search prevents worst-case explosion
- Early filters resolve most/all cases without DFS

CODE TO FPGA TRANSLATION

The Hardcaml design translates to synthesizable RTL:

1. Always.State_machine creates explicit FSM with state registers
2. Always.Variable.reg creates flip-flops for sequential state
3. Ram.create generates BRAM inference (Block RAM on FPGA)
4. Combinational logic from signal operations (mux, arithmetic, bitwise)
5. Hierarchical module structure preserved in Verilog

The generated Verilog can be synthesized to FPGA using standard tools (Vivado
for Xilinx, Quartus for Intel/Altera). Resource usage scales with:

- BRAM: ~1-2 blocks for placement_ram (4096 x 64 bits)
- Registers: ~32 * 64 + state machine overhead
- Logic: moderate (mostly muxes and comparators)

FPGA OPTIMIZATIONS

1. Host pre-computation eliminates need for division and multiplication in hardware
2. Early filters avoid DFS entirely for most cases
3. Bitmask representation enables single-cycle overlap detection
4. Bounded DFS with step counter ensures deterministic runtime
5. BRAM usage minimizes register count for large placement storage
6. Explicit FSM avoids recursive call overhead

RESULTS

The hardware solver correctly computes 587 regions that can fit their shapes.
The DFS implementation has been verified on small test cases and works correctly
for boards up to 64 bits (e.g., 4x4, 8x8).
