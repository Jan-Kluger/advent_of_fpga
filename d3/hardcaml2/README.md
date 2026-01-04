Joltage Finder - Hardcaml Implementation
What it does

Each input line has exactly 100 decimal digits. The algorithm selects 12 digits (removing 88) to produce the largest possible number. The puzzle answer for Part 2 was 170418192256861.

For example, from "111111...987654321111" we'd keep "987654321111" by removing 88 ones from the front.

The algorithm

Uses a monotonic decreasing stack approach:

1. Process digits one at a time
2. For each new digit, while the stack top is smaller AND we haven't removed 88 digits yet, pop it
3. Push the new digit
4. After all 100 digits, take the first 12 from the stack

This greedy approach handles backtracking via pops. A larger digit "kicks out" smaller digits that came before it, as long as we still have removal budget left.

Hardware architecture

The core is an FSM with these states:

- Idle: waiting for first digit of a bank
- WaitDigit: waiting for next digit (keeps digit_count)
- Process: decide whether to pop or push
- Pop: decrement stack pointer, read new top
- ReadTop: RAM read latency cycle
- Push: write digit to stack RAM
- ReadAccum/Accumulate: build the 12-digit decimal result

Key design decisions:

1. Block RAM for stack storage (128 entries x 4 bits). This avoids the 100-to-1 mux nightmare that was causing circuit elaboration timeouts.

2. Separate WaitDigit state. The simulation interface sends one digit per cycle then waits for busy=0. We needed a "waiting but not reset" state to keep digit_count across digits.

3. Early-idle optimization. Once we've removed exactly 88 digits AND have exactly 12 on the stack, no more pops are possible. We skip the pop check and just count remaining digits.

Dataflow

Input comes as 4-bit digits with a valid strobe. The hardware:
1. Streams digits into the stack via the monotonic algorithm
2. After 100 digits, reads stack[0..11] to build the result
3. Accumulates: result = result*10 + digit for each of 12 digits
4. Adds bank result to running sum
5. Signals bank_done, returns to Idle         # runs expect tests

The main binary reads input.txt and prints the Part 2 answer.

Files

- hw/joltage_core.ml - the synthesizable hardware module
- sim/sim_core.ml - simulation driver and OCaml reference implementation
- test/test_pattern_finder.ml - expect tests comparing hardware vs reference
- gen/emit_verilog.ml - generates Verilog RTL output

Performance notes

Circuit elaboration was the main challenge. Initial attempts with register arrays and giant muxes caused OCaml to timeout during circuit building. Block RAM solved this completely. The circuit now builds in under a second and simulates 200 banks quickly.

The FSM processes roughly 2-4 cycles per digit on average (depends on pop frequency), plus 24 cycles for accumulation. Total per bank is around 200-400 cycles.
