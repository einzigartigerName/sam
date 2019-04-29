module BasicAsm : Asm.Asm with type mem = int and type reg = int = struct

(* -------------------- Types -------------------- *)
    type label = string * int
    type mem = int
    type reg = int
    type const = int

    type call = 
        (* Move *)
        | MOVRR of reg * reg
        | MOVRM of reg * mem
        | MOVMR of mem * reg
        | MOVRC of reg * const
        | MOVMC of mem * const
        (* Add *)
        | ADDRR of reg * reg
        | ADDRM of reg * mem
        (* sub *)
        | SUBRR of reg * reg
        | SUBRM of reg * mem
        (* mul *)
        | MULRR of reg * reg
        | MULRM of reg * mem
        (* div *)
        | DIVRR of reg * reg
        | DIVRM of reg * mem
        (* and *)
        | ANDRR of reg * reg
        | ANDRM of reg * mem
        (* or *)
        | ORRR of reg * reg
        | ORRM of reg * mem
        (* NOT *)
        | NOTR of reg
        (* Compare *)
        | CMPRR of reg * reg
        | CMPRM of reg * mem
        (* JUMP *)
        | JMP of int
        (* JUMP Zero *)
        | JZ of int
        (* JUMP Greater or Equal *)
        | JGE of int
        (* JUMP Greater *)
        | JG of int
        (* JUMP Less or Equal *)
        | JLE of int
        (* JUMP Less *)
        | JL of int
        (* PUSH *)
        | PUSH of reg
        (* POP *)
        | POP of reg    
        (* LABEL *)
        | LABEL of int
        (* NUL *)
        | NUL
        (* HALT *)
        | HALT

(* -------------------- Utils -------------------- *)
    (* type for type-parsing *)
    type arg_type =
        | CONST of const
        | REG of reg
        | MEM of mem

    (* "Const" *)
    let label_regex = Str.regexp "^[a-z_][a-z0-9_]+:$"
    let mem_regex = Str.regexp "\[[0-9]+\]"
    let first = Str.regexp "\["
    let last = Str.regexp "\]"
    (* Error in parsing *)
    let error line_num line = Printf.printf "ERROR in line %d: %s\n" line_num line


    (* parse argument into register/memory-address *)
    let parse_arg v =
        try
            CONST (int_of_string v)
        with e -> 
            if Str.string_match mem_regex v 0 then
                CONST (int_of_string (
                        Str.global_replace last "" (
                            Str.global_replace first "" v)))
            else
            match v with
            | "a" -> REG 0
            | "b" -> REG 1
            | "c" -> REG 2
            | "d" -> REG 3
            | "e" -> REG 4
            | "f" -> REG 5
            | _ -> Printf.printf "ERROR: register \'%s\' not found" v; exit 0

    (* check for label *)
    let detect_label file =
        let fc = open_in file in
        let rec impl acc count =
            try
                let line = String.lowercase_ascii (input_line fc)
                in
                if Str.string_match label_regex line 0
                    then impl ((line, count ) :: acc) (count + 1)
                    else impl acc (count + 1)

            with End_of_file -> close_in fc; acc
        in
        List.rev (impl [] 1)

    (* substitute label with line *)
    let slwl label li =
        let rec impl = function
            | [] -> Printf.printf "ERROR: Label \'%s\' not found" label; exit 0
            | (l, i)::tl -> if label = l then i else impl tl
        in
        try
            int_of_string label
        with e -> impl li

    (* list to array *)
    let list_to_array li zero =
        let array = Array.make (List.length li) zero in
        let rec impl i = function
            | [] -> array
            | h::tl -> array.(i) <- h; impl (i + 1) tl
        in
        impl 0 li

(* -------------------- Implementation -------------------- *)
    let parse file =
        let labels = detect_label file
        in
        let fc = open_in file
        in
        let rec impl acc count =
            try
                let line = String.lowercase_ascii (input_line fc)
                in
                let split = String.split_on_char ' ' line
                in
                (* Check if line is label *)
                if Str.string_match label_regex line 0
                then impl (LABEL count :: acc) (count + 1)
                (* Parse line *)
                else match split with
                        (* Jumps *)
                        | "jmp"::a::[] -> impl ((JMP (slwl a labels))::acc) (count + 1)
                        | "jge"::a::[] -> impl ((JGE (slwl a labels))::acc) (count + 1)
                        | "jg"::a::[] -> impl ((JG (slwl a labels))::acc) (count + 1)
                        | "jle"::a::[] -> impl ((JLE (slwl a labels))::acc) (count + 1)
                        | "jl"::a::[] -> impl ((JL (slwl a labels))::acc) (count + 1)
                        | "jz"::a::[] -> impl ((JZ (slwl a labels))::acc) (count + 1)
                        (* Halt *)
                        | "halt"::[] -> impl (HALT::acc) (count +1)
                        (* empty line *)
                        | ""::[] -> impl (NUL::acc) (count + 1)
                        (* Compare *)
                        | "cmp"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((CMPRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((CMPRM (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* ADD *)
                        | "add"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((ADDRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((ADDRM (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* SUB *)
                        | "sub"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((SUBRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((SUBRM (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* MUL *)
                        | "mul"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((MULRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((MULRM (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* DIV *)
                        | "div"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((DIVRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((DIVRM (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* MOV *)
                        | "mov"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((MOVRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((MOVRM (x, y))::acc) (count + 1)
                                | MEM x, REG y -> impl ((MOVMR (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((MOVRC (x, y))::acc) (count + 1)
                                | MEM x, CONST y -> impl ((MOVMC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        | _ -> error (count + 1) line; exit 0
            with End_of_file ->
                close_in fc;
                acc
        in
        list_to_array (List.rev (impl [] 0)) NUL


    let execute mem prog =
        (* flags: g - z - l *)
        let flags = Array.make 3 false in
        let reset_flags = 
            flags.(0) <- false;
            flags.(1) <- false;
            flags.(2) <- false;
        in
        let set_flags = function
            | x when x < 0 -> reset_flags; flags.(2) <- true
            | x when x = 0 -> reset_flags; flags.(1) <- true
            | x when x > 0 -> reset_flags; flags.(0) <- true
            | _ -> Printf.printf "ERROR: unknown state from comparison\n"; exit 0
        in
        let register = Array.make 6 0 in
        let memory = Array.make mem 0 in
        let stack = Stack.create () in
        let rec impl pc =
            match prog.(pc) with
                | MOVRR (a, b) -> register.(a) <- register.(b); impl (pc + 1)
                | MOVRM (a, b) -> register.(a) <- memory.(b); impl (pc + 1)
                | MOVMR (a, b) -> memory.(a) <- register.(b); impl (pc + 1)
                | MOVRC (a, b) -> register.(a) <- b; impl (pc + 1)
                | MOVMC (a, b) -> memory.(a) <- b; impl (pc + 1)
                | ADDRR (a, b) -> register.(a) <- register.(a) + register.(b); impl (pc + 1)
                | ADDRM (a, b) -> register.(a) <- register.(a) + memory.(b); impl (pc + 1)
                | SUBRR (a, b) -> register.(a) <- register.(a) - register.(b); impl (pc + 1)
                | SUBRM (a, b) -> register.(a) <- register.(a) - memory.(b); impl (pc + 1)
                | MULRR (a, b) -> register.(a) <- register.(a) * register.(b); impl (pc + 1)
                | MULRM (a, b) -> register.(a) <- register.(a) * memory.(b); impl (pc + 1)
                | DIVRR (a, b) -> register.(a) <- register.(a) / register.(b); impl (pc + 1)
                | DIVRM (a, b) -> register.(a) <- register.(a) / memory.(b); impl (pc + 1)
                | CMPRR (a, b) ->
                    let cmp = compare register.(a) register.(b) in
                    set_flags cmp; impl (pc + 1)
                | CMPRM (a, b) ->
                    let cmp = compare register.(a) memory.(b) in
                    set_flags cmp; impl (pc + 1)
                | JMP i -> impl (i - 1)
                | JGE i -> if flags.(0) || flags.(1) then impl (i + 1) else impl (pc + 1)
                | JG i -> if flags.(1) then impl (i + 1) else impl (pc + 1)
                | JLE i -> if flags.(2) || flags.(1) then impl (i + 1) else impl (pc + 1)
                | JL i -> if flags.(2) then impl (i + 1) else impl (pc + 1)
                | JZ i -> if flags.(1) then impl (i + 1) else impl (pc + 1)
                | LABEL _ | NUL -> impl (pc + 1)
                | PUSH r -> Stack.push register.(r) stack; impl (pc + 1)
                | POP r -> register.(r) <- Stack.pop stack; impl (pc + 1)
                | HALT -> register.(0)
                | _ -> Printf.printf "ERROR in line %d" pc; exit 0
        in
        impl 0
end