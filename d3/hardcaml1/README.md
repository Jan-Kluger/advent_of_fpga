Given banks of batteries (lines of digits 1-9), find max 2-digit number by picking two digits at positions i < j. joltage = 10*digit[i] + digit[j]. Sum all bank maxima.

Algorithm: single left-to-right pass per bank. best_tens = max digit seen so far, best_val = max 2-digit value seen so far. For each digit d: candidate = 10*best_tens + d, update best_val and best_tens. On newline: accumulate best_val into sum, reset.

2-stage pipeline with forwarding. Stage C computes candidate = 10*best_tens_fwd + digit, where best_tens_fwd is forwarded from stage W to avoid stale data. Pipeline registers hold candidate_p, digit_p, valid_p, newline_p between stages.

Stage W updates architectural state: best_val = max(best_val, candidate_p), best_tens = max(best_tens, digit_p). On newline_p, adds best_val to sum and resets.

Forwarding logic for best_tens_fwd: if newline_p return 0 (resetting for next bank), if valid_p return new_best_tens (bypass from current W stage computation), else return best_tens register.

The newline case is critical: when we reset best_tens, stage C is simultaneously computing a candidate for the next bank's first digit. Without forwarding 0, that candidate would incorrectly use the previous bank's final best_tens.

Files:
- hw/joltage_core.ml - the synthesizable hardware module
- sim/sim_core.ml - simulation driver
- test/ - expect tests
- gen/emit_verilog.ml - generates Verilog RTL output
