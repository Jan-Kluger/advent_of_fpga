Paper Roll Removal - Hardcaml FPGA Implementation

This project solves Part 2 of a grid-based puzzle where "paper rolls" (@ symbols)
must be removed from a grid. A roll can be removed if it has fewer than 4 neighbors
(8-connectivity). Removing a roll may cause neighbors to become removable, creating
a cascade effect. The goal is to count total removals.


ALGORITHM

The algorithm is k-core graph peeling:

1. Parse input grid, compute initial neighbor count for each cell
2. Enqueue all cells with degree < 4 into a work queue
3. While queue not empty:
   - Pop cell from queue
   - If still alive, mark dead and increment count
   - For each neighbor: decrement degree, enqueue if degree drops to 3
4. Return total count


ARCHITECTURE

The design has two phases running sequentially:

Phase 1 - Streaming Initialization (init_stream.ml)
  - Streams input bytes through 3x3 sliding window using two line buffers
  - Computes neighbor count for each cell using tree adder
  - Writes cell state (alive + degree) to Grid BRAM
  - Pushes initially removable cells (degree < 4) to FIFO

Phase 2 - Peeling FSM (peel_fsm.ml)  
  - Pops cells from FIFO
  - Reads cell from BRAM, skips if already dead
  - Marks cell dead, increments removal counter
  - For each of 8 neighbors: read, decrement degree, push to FIFO if now removable


MODULE STRUCTURE

grid_bram.ml
  Simple dual-port BRAM storing 5 bits per cell (1 alive + 4 degree)
  Up to 160x160 = 25600 cells

fifo_queue.ml  
  BRAM-backed FIFO storing (row, col) pairs
  Head/tail pointer management

init_stream.ml
  Streaming 3x3 convolution with line buffer BRAMs
  Two-stage pipeline for timing closure
  Outputs BRAM writes and FIFO pushes

peel_fsm.ml
  12-state FSM handling BRAM read latency
  Processes 8 neighbors per removed cell
  Bounds checking for edge cells

paper_roll_part2.ml
  Top-level module wiring submodules together
  Phase state machine (Init -> Peel -> Done)


FPGA OPTIMIZATIONS

1. BRAM Usage
   - Cell state stored in block RAM, not registers
   - Line buffers use BRAM for 160-wide rows
   - FIFO uses BRAM instead of shift register

2. Streaming Input
   - No need to buffer entire grid before processing
   - Line buffers hold only 2 rows at a time
   - Neighbor count computed combinationally from 3x3 window

3. Pipelined Neighbor Sum
   - Uses tree reduction with arity 2
   - Balances adder tree for timing

4. Sequential Neighbor Processing
   - Processes 8 neighbors one at a time
   - Trades throughput for reduced BRAM ports
   - Single read + write port sufficient

5. Address Calculation
   - Flat addressing: addr = row * width + col
   - Avoids division by storing (row, col) in FIFO


SIMULATION

Build and run:
  dune build
  dune exec paper_roll_finder

Run tests:
  dune runtest

Generate Verilog:
  dune exec emit_verilog


RESULTS

Input: 140x140 grid with ~10000 paper rolls
Output: 8013 (total removals in Part 2)
