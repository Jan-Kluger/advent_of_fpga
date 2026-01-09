This solves Part 2 of a grid-based puzzle where "paper rolls" (@ symbols) must be removed from a grid. A roll can be removed if it has fewer than 4 neighbors (8-connectivity). Removing a roll may cause neighbors to become removable, creating a cascade effect. The goal is to count total removals.

The algorithm is k-core graph peeling. Parse input grid and compute initial neighbor count for each cell. Enqueue all cells with degree < 4 into a work queue. While queue not empty: pop cell from queue, if still alive mark dead and increment count, for each neighbor decrement degree and enqueue if degree drops to 3. Return total count.

The design has two phases running sequentially. Phase 1 - Streaming Initialization: streams input bytes through 3x3 sliding window using two line buffers, computes neighbor count for each cell using tree adder, writes cell state (alive + degree) to Grid BRAM, pushes initially removable cells (degree < 4) to FIFO. Phase 2 - Peeling FSM: pops cells from FIFO, reads cell from BRAM and skips if already dead, marks cell dead and increments removal counter, for each of 8 neighbors reads, decrements degree, pushes to FIFO if now removable.

Module structure: grid_bram.ml is simple dual-port BRAM storing 5 bits per cell (1 alive + 4 degree), up to 160x160 = 25600 cells. fifo_queue.ml is BRAM-backed FIFO storing (row, col) pairs with head/tail pointer management. init_stream.ml does streaming 3x3 convolution with line buffer BRAMs, two-stage pipeline for timing closure, outputs BRAM writes and FIFO pushes. peel_fsm.ml is a 12-state FSM handling BRAM read latency, processes 8 neighbors per removed cell, bounds checking for edge cells. paper_roll_part2.ml is top-level module wiring submodules together with phase state machine (Init -> Peel -> Done).

FPGA optimizations: cell state stored in block RAM not registers, line buffers use BRAM for 160-wide rows, FIFO uses BRAM instead of shift register. No need to buffer entire grid before processing, line buffers hold only 2 rows at a time, neighbor count computed combinationally from 3x3 window. Uses tree reduction with arity 2 for pipelined neighbor sum. Processes 8 neighbors one at a time, trades throughput for reduced BRAM ports, single read + write port sufficient. Flat addressing: addr = row * width + col, avoids division by storing (row, col) in FIFO.

Files:
- hw/grid_bram.ml - BRAM for cell state storage
- hw/fifo_queue.ml - BRAM-backed FIFO
- hw/init_stream.ml - Streaming initialization phase
- hw/peel_fsm.ml - Peeling FSM phase
- hw/paper_roll_part2.ml - Top-level wiring
- sim/sim_core.ml - Simulation helpers
- test/ - Expect tests
- gen/ - Verilog generation
