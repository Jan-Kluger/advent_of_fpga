The problem: same dial as part 1, numbered 0 to 99, starting at 50. But now
we count how many times the dial crosses through zero, not just lands on it.

If you're at position 30 and rotate left 150 clicks, you pass through zero
once (around click 30) and end up at position 80. If you rotate 250 clicks,
you pass through zero twice. The dial can wrap multiple times per instruction.

Architecture

Two main modules: Zero_crossing_calc and Dial_tracker.

Zero_crossing_calc is purely combinational. Given the current dial position,
direction, and number of clicks, it computes two things:

1. How many times the dial crosses zero during this move
2. The new dial position after the move

The crossing count depends on direction:

- Going left from position P with N clicks: you cross zero if N >= P,
  then once more for every additional 100 clicks after that
- Going right from position P: you cross zero if N >= (100 - P),
  same logic for additional crossings

Dial_tracker wraps Zero_crossing_calc with registers. It keeps the running
dial position and accumulated crossing count across clock cycles. Each cycle
when valid is high, it feeds the new instruction to Zero_crossing_calc,
updates the position, and adds any crossings to the total.

The division problem

Computing "how many times did we wrap" means dividing by 100. Hardware
division is expensive, you either need iterative logic (slow) or a lot
of combinational depth.

We use the magic constant trick instead. For any constant divisor, you can
find a multiplier M and shift amount S such that x/d = (x\*M) >> S. For
dividing by 100 with 16-bit inputs:

x / 100 = (x \* 5243) >> 19

This gives exact results for all values we care about. One 16x16 multiply
and a bit shift replaces what would otherwise be a divider circuit.

Pipelining

There's also a Dial_tracker_pipelined variant that registers the inputs
before processing. This breaks up the critical path for higher clock speeds
at the cost of one cycle of latency. For streaming data where throughput
matters more than latency, the pipelined version is better.

Files

hw/core.ml - Zero_crossing_calc, Dial_tracker, and pipelined variant
hw/top.ml - Top-level wiring with option for pipelined/flat versions
hw/top_intf.ml - Port definitions
sim/sim_core.ml - Parses input and drives simulation
sim/sim_top.ml - Simulation entry point
gen/emit_verilog.ml - Verilog generation with -pipelined flag
test/ - Unit tests with waveform output
