Each input line has exactly 100 decimal digits. The algorithm selects 12 digits (removing 88) to produce the largest possible number.

Uses a monotonic decreasing stack approach. Process digits one at a time. For each new digit, while the stack top is smaller AND we haven't removed 88 digits yet, pop it. Push the new digit. After all 100 digits, take the first 12 from the stack. This greedy approach handles backtracking via pops. A larger digit "kicks out" smaller digits that came before it, as long as we still have removal budget left.

The core is an FSM with these states: Idle (waiting for first digit of a bank), WaitDigit (waiting for next digit, keeps digit_count), Process (decide whether to pop or push), Pop (decrement stack pointer, read new top), ReadTop (RAM read latency cycle), Push (write digit to stack RAM), ReadAccum/Accumulate (build the 12-digit decimal result).

Key design decisions: Block RAM for stack storage (128 entries x 4 bits) to avoid the 100-to-1 mux nightmare that was causing circuit elaboration timeouts. Separate WaitDigit state because the simulation interface sends one digit per cycle then waits for busy=0, so we needed a "waiting but not reset" state to keep digit_count across digits. Early-idle optimization: once we've removed exactly 88 digits AND have exactly 12 on the stack, no more pops are possible, so we skip the pop check and just count remaining digits.

Input comes as 4-bit digits with a valid strobe. The hardware streams digits into the stack via the monotonic algorithm, after 100 digits reads stack[0..11] to build the result, accumulates result = result*10 + digit for each of 12 digits, adds bank result to running sum, signals bank_done, returns to Idle.

Files:
- hw/joltage_core.ml - the synthesizable hardware module
- sim/sim_core.ml - simulation driver and OCaml reference implementation
- test/ - expect tests comparing hardware vs reference
- gen/emit_verilog.ml - generates Verilog RTL output
