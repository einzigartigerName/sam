open Basicasm.BasicAsm

(* util - list to array *)
let list_to_array li zero =
    let array = Array.make (List.length li) zero in
    let rec impl i = function
        | [] -> array
        | h::tl -> array.(i) <- h; impl (i + 1) tl
    in
    impl 0 li

let _ =
    let source =
        try
            Sys.argv.(1)
        with e -> Printf.printf("ERROR: please provide a source file\n"); exit 0
    in
    let prog = parse source in
    let result = execute 1024 prog in
    Printf.printf "Result: %d\n" result