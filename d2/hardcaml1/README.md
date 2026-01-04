This project finds "parallel numbers" (numbers formed by repeating digit patterns) within ranges and sums them. For example, 123123 is a parallel number because it's the pattern 123 repeated twice.

The problem: given ranges like [288352, 412983], find all parallel numbers in each range and sum them.

Algorithm

For a pattern of n digits, the parallel number is pattern * (10^n + 1). For example:

n=1, pattern=5: 5 * 11 = 55
n=2, pattern=12: 12 * 101 = 1212
n=3, pattern=123: 123 * 1001 = 123123

For each pattern length n, valid patterns range from 10^(n-1) to 10^n - 1 (except n=1 which is 1-9).

The current implementation iterates through all valid pattern values for each n, computes the parallel number, checks if it falls within the range [lo, hi], and accumulates matching values.

Current hardware implementation

The hardware is fully synthesizable and uses:

A 3-state FSM (Idle, Iterating, Done)
ROM lookup tables for powers of 10 and multipliers
64-bit arithmetic for pattern computation
Early termination when min_invalid > hi

For each range, the hardware iterates through pattern values, which can be slow for large ranges but is correct and synthesizable.

Planned optimizations

The iteration can be eliminated using O(1) bounds computation:

Pass numbers as digit arrays instead of binary (avoids BCD conversion)
Extract first n digits of lo and hi directly (O(1) array slicing)
Compute lower bound: extract first n digits of lo, compute parallel number, if less than lo then increment by 1
Compute upper bound: extract first n digits of hi, compute parallel number, if greater than hi then decrement by 1
If lower > upper, no parallel numbers exist for this n
Otherwise compute sum using arithmetic series: multiplier * count * (x_lo + x_hi) / 2

This reduces per-range computation from O(max_pattern) cycles to O(1) combinational logic per pattern length, giving approximately 1-10 million times speedup.

Additional FPGA optimizations:

Process all pattern lengths (n=1 to 7) in parallel for single-cycle computation
Use DSP slices for 64-bit multiplications
Pipeline stages if timing is tight

Usage

Build and run:
dune build
dune exec -- pattern_finder

Generate Verilog:
dune exec -- gen/emit_verilog.exe > pattern_finder.v

Run tests:
dune test

Project structure

hw/pattern_finder.ml - synthesizable hardware core
sim/sim_core.ml - simulation helpers
bin/main.ml - CLI that runs hardware simulation
gen/emit_verilog.ml - Verilog RTL generation
test/ - expect tests
