module type Asm = sig
    type mem
    type reg
    type label
    type call

    (* parse: source file -> calls *)
    val parse : string -> call array
    (* execute:  memory size -> calls -> result *)
    val execute : int -> call array -> int
end