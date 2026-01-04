open Hardcaml
open Signal

let digits_to_select = 12
let input_length = 100
let to_remove = input_length - digits_to_select
let stack_depth = 128
let result_bits = 48
let sum_bits = 64

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; digit : 'a [@bits 4]
    ; digit_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { sum : 'a [@bits sum_bits]
    ; bank_done : 'a
    ; busy : 'a
    }
  [@@deriving hardcaml]
end

module State = struct
  type t = Idle | WaitDigit | Process | Pop | ReadTop | Push | Accumulate | ReadAccum
  [@@deriving sexp_of, compare, enumerate]
end

let create (scope : Scope.t) (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in

  let state = State_machine.create (module State) reg_spec in
  
  let write_enable = Variable.wire ~default:gnd in
  let write_addr = Variable.wire ~default:(zero 7) in
  let write_data = Variable.wire ~default:(zero 4) in
  let read_addr = Variable.wire ~default:(zero 7) in
  
  let stack_ram = Ram.create 
    ~collision_mode:Read_before_write 
    ~size:stack_depth
    ~write_ports:[| { write_clock = i.clock
                    ; write_enable = write_enable.value
                    ; write_address = write_addr.value
                    ; write_data = write_data.value } |]
    ~read_ports:[| { read_clock = i.clock
                   ; read_enable = vdd
                   ; read_address = read_addr.value } |]
    ()
  in
  let ram_read_data = stack_ram.(0) in

  let sp = Variable.reg reg_spec ~width:7 in
  let removed = Variable.reg reg_spec ~width:7 in
  let digit_count = Variable.reg reg_spec ~width:7 in
  let current_digit = Variable.reg reg_spec ~width:4 in
  let stack_top = Variable.reg reg_spec ~width:4 in
  let accumulator = Variable.reg reg_spec ~width:result_bits in
  let accum_idx = Variable.reg reg_spec ~width:4 in
  let sum = Variable.reg reg_spec ~width:sum_bits in
  let bank_done = Variable.reg reg_spec ~width:1 in
  let early_idle = Variable.reg reg_spec ~width:1 in

  let can_pop = (sp.value >:. 0) &: (removed.value <:. to_remove) &: (current_digit.value >: stack_top.value) in
  let times_10 x = (sll x 3) +: (sll x 1) in

  compile [
    bank_done <--. 0;
    write_enable <--. 0;
    read_addr <-- sp.value -:. 1;
    
    state.switch [
      Idle, [
        when_ i.digit_valid [
          current_digit <-- i.digit;
          digit_count <--. 1;
          state.set_next Process;
        ];
      ];
      
      WaitDigit, [
        when_ i.digit_valid [
          current_digit <-- i.digit;
          digit_count <-- digit_count.value +:. 1;
          state.set_next Process;
        ];
      ];
      
      Process, [
        if_ early_idle.value [
          if_ (digit_count.value ==:. input_length) [
            accum_idx <--. 0;
            read_addr <--. 0;
            state.set_next ReadAccum;
          ] @@ elif i.digit_valid [
            digit_count <-- digit_count.value +:. 1;
          ] []
        ] @@ elif can_pop [
          state.set_next Pop;
        ] [
          state.set_next Push;
        ]
      ];
      
      Pop, [
        sp <-- sp.value -:. 1;
        removed <-- removed.value +:. 1;
        read_addr <-- sp.value -:. 2;
        state.set_next ReadTop;
      ];
      
      ReadTop, [
        stack_top <-- ram_read_data;
        state.set_next Process;
      ];
      
      Push, [
        write_enable <--. 1;
        write_addr <-- sp.value;
        write_data <-- current_digit.value;
        sp <-- sp.value +:. 1;
        stack_top <-- current_digit.value;
        when_ ((removed.value ==:. to_remove) &: (sp.value ==:. (digits_to_select - 1))) [
          early_idle <--. 1;
        ];
        if_ (digit_count.value ==:. input_length) [
          accum_idx <--. 0;
          read_addr <--. 0;
          state.set_next ReadAccum;
        ] @@ elif i.digit_valid [
          current_digit <-- i.digit;
          digit_count <-- digit_count.value +:. 1;
          state.set_next Process;
        ] [
          state.set_next WaitDigit;
        ]
      ];
      
      ReadAccum, [
        read_addr <-- uresize accum_idx.value 7;
        state.set_next Accumulate;
      ];
      
      Accumulate, (
        let digit_val = uresize ram_read_data result_bits in
        let final_val = times_10 accumulator.value +: digit_val in
        [
          accumulator <-- final_val;
          if_ (accum_idx.value ==:. (digits_to_select - 1)) [
            sum <-- sum.value +: uresize final_val sum_bits;
            bank_done <--. 1;
            sp <--. 0;
            removed <--. 0;
            digit_count <--. 0;
            accumulator <--. 0;
            early_idle <--. 0;
            state.set_next Idle;
          ] [
            accum_idx <-- accum_idx.value +:. 1;
            read_addr <-- uresize (accum_idx.value +:. 1) 7;
            state.set_next ReadAccum;
          ]
        ]);
    ];
  ];

  let busy = ~:((state.is Idle) |: (state.is WaitDigit)) -- "busy" in
  { O.sum = sum.value -- "sum"
  ; bank_done = bank_done.value -- "bank_done"
  ; busy
  }

let hierarchical scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"joltage_core" create i
