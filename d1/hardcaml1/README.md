The problem: you have a dial numbered 0 to 99. It starts at position 50.
You get a list of instructions like "L45" (rotate left 45 clicks) or "R120"
(rotate right 120 clicks). Count how many times the dial lands exactly on zero.

Architecture

The design has two modules: Parser and Counter.

Parser takes each instruction and updates the dial position. It tracks a 7-bit
rotation register (0-99). When a new instruction arrives, it computes the new
position and outputs a pulse on zero_signal if the result is exactly zero.

Counter is just a 16-bit incrementer. Every time it sees zero_signal go high,
it bumps the count.

The data flows like this:

input -> Parser -> zero_signal -> Counter -> count_out

The interesting hardware problem here is computing mod 100 without division.
Since each instruction only moves the dial by at most 99 positions (after
taking the input mod 100), the position can only overflow or underflow by
at most one full rotation. So we don't need general-purpose modulo, we can
just check bounds and wrap:

if position >= 100 then position - 100
if position < 0 then position + 100

We apply this wrapping twice in each direction to handle the edge cases where
the intermediate sum might be out of range by up to 200.

Files

hw/core.ml - Parser and Counter module definitions
hw/top.ml - Wires Parser and Counter together
hw/top_intf.ml - Top-level port definitions (clock, reset, data, etc.)
sim/sim_core.ml - Parses input file and drives the simulation
sim/sim_top.ml - Entry point for running simulation
gen/emit_verilog.ml - Generates Verilog output
test/test_d1.ml - Unit tests with waveform output
