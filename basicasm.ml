module BasicAsm : Asm.Asm with type mem = int and type reg = int = struct

(* -------------------- Types -------------------- *)
    type label = string * int
    type mem = int
    type reg = int
    type const = int

    type call = 
        (* move *)
        | MOVRR of reg * reg
        | MOVRM of reg * mem
        | MOVMR of mem * reg
        | MOVRC of reg * const
        | MOVMC of mem * const
        (* add *)
        | ADDRR of reg * reg
        | ADDRM of reg * mem
        | ADDRC of reg * const
        (* sub *)
        | SUBRR of reg * reg
        | SUBRM of reg * mem
        | SUBRC of reg * const
        (* mul *)
        | MULRR of reg * reg
        | MULRM of reg * mem
        | MULRC of reg * const
        (* div *)
        | DIVRR of reg * reg
        | DIVRM of reg * mem
        | DIVRC of reg * const
        (* and *)
        | ANDRR of reg * reg
        | ANDRM of reg * mem
        | ANDRC of reg * const
        (* or *)
        | ORRR of reg * reg
        | ORRM of reg * mem
        | ORRC of reg * const
        (* not *)
        | NOTR of reg
        (* compare *)
        | CMPRR of reg * reg
        | CMPRM of reg * mem
        | CMPRC of reg * const
        (* jump *)
        | JMP of int
        (* jump zero *)
        | JZ of int
        (* jump greater or equal *)
        | JGE of int
        (* jump greater *)
        | JG of int
        (* jump less or equal *)
        | JLE of int
        (* jump less *)
        | JL of int
        (* push *)
        | PUSH of reg
        (* pop *)
        | POP of reg    
        (* label *)
        | LABEL of int
        (* nul *)
        | NUL
        (* halt *)
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
                        (* PUSH *)
                        | "push"::a::[] ->
                            let arg_a = parse_arg a in
                            (match arg_a with
                                | REG x -> impl ((PUSH x)::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* POP *)
                        | "pop"::a::[] ->
                            let arg_a = parse_arg a in
                            (match arg_a with
                                | REG x -> impl ((POP x)::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
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
                                | REG x, CONST y -> impl ((CMPRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* AND *)
                        | "and"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((ANDRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((ANDRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((ANDRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* OR *)
                        | "or"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((ORRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((ORRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((ORRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* NOT *)
                        | "not"::a::[] ->
                            let arg_a = parse_arg a in
                            (match arg_a with
                                | REG x -> impl ((NOTR x)::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* ADD *)
                        | "add"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((ADDRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((ADDRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((ADDRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* SUB *)
                        | "sub"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((SUBRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((SUBRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((SUBRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* MUL *)
                        | "mul"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((MULRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((MULRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((MULRC (x, y))::acc) (count + 1)
                                | _ -> error (count + 1) line; exit 0)
                        (* DIV *)
                        | "div"::a::b::[] ->
                            let arg_a = parse_arg a in
                            let arg_b = parse_arg b in
                            (match arg_a, arg_b with
                                | REG x, REG y -> impl ((DIVRR (x, y))::acc) (count + 1)
                                | REG x, MEM y -> impl ((DIVRM (x, y))::acc) (count + 1)
                                | REG x, CONST y -> impl ((DIVRC (x, y))::acc) (count + 1)
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
            if pc >= Array.length prog then
                register.(0)
            else match prog.(pc) with
                (* MOVE *)
                | MOVRR (a, b) -> register.(a) <- register.(b); impl (pc + 1)
                | MOVRM (a, b) -> register.(a) <- memory.(b); impl (pc + 1)
                | MOVMR (a, b) -> memory.(a) <- register.(b); impl (pc + 1)
                | MOVRC (a, b) -> register.(a) <- b; impl (pc + 1)
                | MOVMC (a, b) -> memory.(a) <- b; impl (pc + 1)
                (* ADD *)
                | ADDRR (a, b) -> register.(a) <- register.(a) + register.(b); impl (pc + 1)
                | ADDRM (a, b) -> register.(a) <- register.(a) + memory.(b); impl (pc + 1)
                | ADDRC (a, b) -> register.(a) <- register.(a) + b; impl (pc + 1)
                (* SUB *)
                | SUBRR (a, b) -> register.(a) <- register.(a) - register.(b); impl (pc + 1)
                | SUBRM (a, b) -> register.(a) <- register.(a) - memory.(b); impl (pc + 1)
                | SUBRC (a, b) -> register.(a) <- register.(a) - b; impl (pc + 1)
                (* MUL *)
                | MULRR (a, b) -> register.(a) <- register.(a) * register.(b); impl (pc + 1)
                | MULRM (a, b) -> register.(a) <- register.(a) * memory.(b); impl (pc + 1)
                | MULRC (a, b) -> register.(a) <- register.(a) * b; impl (pc + 1)
                (* DIV *)
                | DIVRR (a, b) -> register.(a) <- register.(a) / register.(b); impl (pc + 1)
                | DIVRM (a, b) -> register.(a) <- register.(a) / memory.(b); impl (pc + 1)
                | DIVRC (a, b) -> register.(a) <- register.(a) / b; impl (pc + 1)
                (* CMP *)
                | CMPRR (a, b) ->
                    let cmp = compare register.(a) register.(b) in
                    set_flags cmp; impl (pc + 1)
                | CMPRM (a, b) ->
                    let cmp = compare register.(a) memory.(b) in
                    set_flags cmp; impl (pc + 1)
                (* AND *)
                | ANDRR (a, b) -> register.(a) <- register.(a) land register.(b); impl (pc + 1)
                | ANDRM (a, b) -> register.(a) <- register.(a) land memory.(b); impl (pc + 1)
                | ANDRC (a, b) -> register.(a) <- register.(a) land b; impl (pc + 1)
                (* OR *)
                | ORRR (a, b) -> register.(a) <- register.(a) lor register.(b); impl (pc + 1)
                | ORRM (a, b) -> register.(a) <- register.(a) lor memory.(b); impl (pc + 1)
                | ORRC (a, b) -> register.(a) <- register.(a) lor b; impl (pc + 1)
                (* NOT *)
                | NOTR a -> register.(a) <- lnot register.(a); impl (pc + 1)
                (* JUMP *)
                | JMP i -> impl (i - 1)
                | JGE i -> if flags.(0) || flags.(1) then impl (i + 1) else impl (pc + 1)
                | JG i -> if flags.(1) then impl (i + 1) else impl (pc + 1)
                | JLE i -> if flags.(2) || flags.(1) then impl (i + 1) else impl (pc + 1)
                | JL i -> if flags.(2) then impl (i + 1) else impl (pc + 1)
                | JZ i -> if flags.(1) then impl (i + 1) else impl (pc + 1)
                (* LABEL / NUL *)
                | LABEL _ | NUL -> impl (pc + 1)
                (* PUSH *)
                | PUSH r -> Stack.push register.(r) stack; impl (pc + 1)
                (* POP *)
                | POP r -> register.(r) <- Stack.pop stack; impl (pc + 1)
                (* HALT *)
                | HALT -> register.(0)
                (* ERROR *)
                | _ -> Printf.printf "ERROR in line %d" pc; exit 0
        in
        impl 0
end