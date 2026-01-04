This project finds "repeated pattern numbers" within ranges and sums them, excluding duplicates. A repeated pattern number is formed by repeating a base pattern k>=2 times. For example, 123123 (123 twice), 121212 (12 three times), and 1111 (1 four times) are all repeated pattern numbers.

The problem: given ranges like [288352, 412983], find all unique repeated pattern numbers and sum them, counting each number only once even if it can be formed multiple ways (e.g., 1111 = 1x4 or 11x2).

Algorithm

For a pattern x with p digits repeated k times, the repeated number is:

ID = x \* MULT(p,k)

where MULT(p,k) = (10^(p\*k) - 1) / (10^p - 1) is the geometric series 1 + 10^p + 10^(2p) + ...

Examples:
p=2, k=2, x=12: 12 _ 101 = 1212
p=3, k=2, x=123: 123 _ 1001 = 123123
p=1, k=4, x=1: 1 \* 1111 = 1111

Deduplication via primitive check

A number like 1111 can be generated as (p=1,k=4,x=1) or (p=2,k=2,x=11). To count each ID exactly once, we only accept "primitive" base patterns, which are patterns that are not themselves repetitions of shorter patterns.

For x with p digits, x is non-primitive if there exists a proper divisor d of p such that x = y * MULT(d, p/d) for some d-digit number y. For example, x=11 with p=2 is non-primitive because 11 = 1 * 11 = 1 * MULT(1,2).

The hardware checks primitiveness using 64-cycle binary restoring division to test divisibility.

Hardware implementation

The design uses an 8-state FSM:
Idle -> Load_params -> Iterate_x -> Prim_div -> Prim_eval -> Next_k -> Next_p -> Done

Key components:
ROM lookup tables for powers of 10 and MULT values (precomputed)
64-bit binary restoring division for primitive checking (64 cycles per divisor)
Nested (p, k) iteration with early termination when IDs exceed range
64-bit arithmetic throughout

Optimizations employed:
Binary division (64 cycles) instead of iterative subtraction (potentially thousands)
Early skip when ID < lo (just increment x)
Early termination when ID > hi (advance to next p,k)
p=1 patterns skip primitive check entirely (always primitive)
Early exit from primitive check on first non-primitive match

Usage

Build and run:
dune build
dune exec \_build/default/bin/main.exe

Run tests:
dune runtest

Generate Verilog:
dune exec gen/emit_verilog.exe > pattern_finder.v

Project structure

hw/pattern_finder.ml - synthesizable hardware core
sim/sim_core.ml - simulation helpers
bin/main.ml - CLI that runs hardware simulation
gen/emit_verilog.ml - Verilog RTL generation
test/ - expect tests with reference implementation
