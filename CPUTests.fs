module Tests

open NUnit.Framework
open FsUnit
open CPU

let runCodeInEmptyCPU code =
    (convertCodeLinesToInstructions code |> Option.get, emptyCPU) ||> runCPU

let runCodeInEmptyCPUAndExpectResult code result =
    runCodeInEmptyCPU code |> should equal result

let [<Test>] canCreateEmptyCPU () = 
    { instructionPointer=0 ; cpuStack=List.empty ; lastOp=None ; globalVariables = Map.empty ; faultyState=false ; errorMessage = "" } |> should equal emptyCPU

let [<Test>] canPushToStack () =
    pushToStack emptyCPU 1 |> should equal { emptyCPU with cpuStack=[1] }

let [<Test>] canPopFromStack () =
    let (_, cpu) = popFromStack 1 { emptyCPU with cpuStack=[1] }
    cpu |> should equal emptyCPU

let [<Test>] canIncreaseValueInStack () =
    let c = runCodeInEmptyCPU [ "PUSH 0" ; "INC" ]
    c |> should equal { c with cpuStack=[1] }

let [<Test>] canDoLoop () =
    let code = [ "PUSH 0" ; "INC" ; "DUP" ; "PUSH 5" ; "CMP" ; "JMPL 1" ]
    let res = (convertCodeLinesToInstructions code |> Option.get, emptyCPU) ||> runCPU
    res |> should equal { res with cpuStack=[5] }
