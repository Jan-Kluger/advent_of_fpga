This finds "parallel numbers" (numbers formed by repeating digit patterns) within ranges and sums them. For example, 123123 is a parallel number because it's the pattern 123 repeated twice.

For a pattern of n digits, the parallel number is pattern * (10^n + 1). For example: n=1, pattern=5 gives 5 * 11 = 55. n=2, pattern=12 gives 12 * 101 = 1212. n=3, pattern=123 gives 123 * 1001 = 123123.

For each pattern length n, valid patterns range from 10^(n-1) to 10^n - 1 (except n=1 which is 1-9).

The hardware iterates through all valid pattern values for each n, computes the parallel number, checks if it falls within the range [lo, hi], and accumulates matching values.

The hardware is fully synthesizable and uses a 3-state FSM (Idle, Iterating, Done), ROM lookup tables for powers of 10 and multipliers, 64-bit arithmetic for pattern computation, and early termination when min_invalid > hi.

Files:
- hw/pattern_finder.ml - synthesizable hardware core
- sim/sim_core.ml - simulation helpers
- bin/main.ml - CLI that runs hardware simulation
- gen/emit_verilog.ml - Verilog RTL generation
- test/ - expect tests
