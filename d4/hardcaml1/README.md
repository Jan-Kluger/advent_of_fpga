Given a grid of cells where '@' marks a filled cell and '.' marks empty, count how many '@' cells are "accessible". A cell is accessible if fewer than 4 of its 8 Moore neighbors (horizontal, vertical, diagonal) are also '@'. Out-of-bounds neighbors are treated as empty.

True streaming implementation with full-edge padding. The hardware processes one cell per clock cycle using only 2 line buffers (not full grid storage). The simulation pads the input with '.' on all four edges: 1 top row of padding, left and right padding columns on each row, 2 bottom rows of padding. This padding ensures out-of-bounds neighbors are naturally zero without needing boundary detection logic in hardware. The hardware just checks column bounds (width known after first row). Row bounds are handled by padding rows having center='.' which makes them non-accessible.

Data structures: two line buffers store previous 2 rows for vertical neighbor access, three 3-element shift registers form 3x3 sliding window, 2-stage pipeline for RAM read latency.

Pipeline stages: receive byte and issue RAM read for line buffers, update sliding window from line buffers and shift registers, evaluate center cell of window and accumulate count.

Memory usage: O(width) using 2 line buffers (~300 bits for 150-wide grid) vs O(width * height) for full grid storage (~33,000 bits).

FPGA optimizations: line buffers use small dual-port BRAMs (150 entries x 1 bit each) instead of large grid storage, read-before-write collision mode. No boundary logic needed since padding eliminates edge case handling. The window naturally contains zeros for out-of-bounds neighbors. Neighbor count uses tree adder with arity 2 for efficient popcount. After 2-row warmup, processes one cell per cycle continuously.

Files:
- hw/paper_roll_core.ml - Synthesizable hardware core (streaming, line buffers)
- sim/sim_core.ml - Simulation helpers with padding injection
- test/ - Expect tests comparing hardware vs reference
- gen/ - Verilog generation
- bin/ - Main executable
