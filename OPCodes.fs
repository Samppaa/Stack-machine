module OPCodes

type Operation =  Nop
                | Push
                | Pop
                | Add
                | Cmp
                | Jmpl
                | Inc
                | Dup
                | LoadGlobal
                | StoreGlobal

let getInstructionFromString instruction =
    match instruction with
    | "PUSH"    -> Some Push
    | "POP"     -> Some Pop
    | "NOP"     -> Some Nop
    | "ADD"     -> Some Add
    | "CMP"     -> Some Cmp
    | "JMPL"    -> Some Jmpl
    | "INC"     -> Some Inc
    | "DUP"     -> Some Dup
    | "LGLOBAL" -> Some LoadGlobal
    | "SGLOBAL" -> Some StoreGlobal
    | _         -> None