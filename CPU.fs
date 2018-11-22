module CPU
open System
open OPCodes

type CPU = { instructionPointer: int
             cpuStack: int list
             lastOp: int option
             globalVariables: Map<int, int>
             faultyState: bool
             errorMessage: string }

type Instruction = { opCode: Operation
                     opPopCount: int
                     opParamCount: int
                     handler: int*int -> int list -> CPU -> CPU }

type OperationAndParams = { operation: Operation
                            opCodeParams: int list }

type Code = { remainingOperations: OperationAndParams list
              doneOperations: OperationAndParams list
              cache: Map<int, OperationAndParams list * OperationAndParams list> }


let errorMessage fmt = sprintf fmt
let setCPUToErrorState cpu errorMessage = 
    { cpu with faultyState = true ; errorMessage = errorMessage }

let pushToStack cpu arg =
    { cpu with cpuStack=arg::cpu.cpuStack }

let jumpToLocation cpu location =
    { cpu with instructionPointer = location }

let addGlobalVariable address variable cpu =
    let globalVars = (address, variable, cpu.globalVariables) |||> Map.add
    { cpu with globalVariables = globalVars }

let retrieveGlobalVariable address cpu =
    let variable = Map.tryFind address cpu.globalVariables
    match variable with
    | None          -> 
        (cpu, errorMessage "(%i) Trying to load global variable from address %i that doesn't exist" cpu.instructionPointer address) ||> setCPUToErrorState
    | Some variable -> 
        pushToStack cpu variable

let popFromStack amount cpu =
    match amount > cpu.cpuStack.Length with
    | true  -> 
        (None, (cpu, sprintf "(%i) Trying to pop %i items but stack only has %i" cpu.instructionPointer amount cpu.cpuStack.Length) ||> setCPUToErrorState)
    | false -> 
        let (a,b) = cpu.cpuStack |> List.splitAt amount
        (Some a, { cpu with cpuStack=b })

let pushFunction popped opCodeParams cpu = 
    List.head opCodeParams |> pushToStack cpu

let popFunction _ _ cpu = cpu
let nopFunction _ _ cpu = cpu

let cmpFunction popped _ cpu =
    match popped with
    | (p1, p2) when p1 > p2 -> pushToStack cpu 1
    | (p1, p2) when p1 = p2 -> pushToStack cpu 0
    | _                     -> pushToStack cpu -1

let addFunction (p1, p2) _ cpu = p1 + p2 |> pushToStack cpu
let incFunction (p1, _) _ cpu = p1 + 1 |> pushToStack cpu
let dupFunction (p1, _) _ cpu = (pushToStack cpu p1, p1) ||> pushToStack

let jmplFunction (p1, _) opCodeParams cpu =
    match p1 with
    | p1 when p1 = 1 -> List.head opCodeParams |> jumpToLocation cpu
    | _              -> cpu

let storeGlobalFunction (p1, _) opCodeParams cpu = (List.head opCodeParams, p1, cpu) |||> addGlobalVariable
let loadGlobal (_, _) opCodeParams cpu = (List.head opCodeParams, cpu) ||> retrieveGlobalVariable

let allInstructions = [ { opCode=Nop ; opPopCount=0 ; opParamCount=0 ; handler=nopFunction };
                        { opCode=Push ; opPopCount=0 ; opParamCount=1 ; handler=pushFunction };
                        { opCode=Pop ; opPopCount=1 ; opParamCount=0 ; handler=popFunction };
                        { opCode=Add ; opPopCount=2 ; opParamCount=0 ; handler=addFunction };
                        { opCode=Cmp ; opPopCount=2 ; opParamCount=0 ; handler=cmpFunction };
                        { opCode=Jmpl ; opPopCount=1 ; opParamCount=1 ; handler=jmplFunction };
                        { opCode=Inc ; opPopCount=1 ; opParamCount=0 ; handler=incFunction };
                        { opCode=Dup ; opPopCount=1 ; opParamCount=0 ; handler=dupFunction };
                        { opCode=LoadGlobal ; opPopCount=0 ; opParamCount=1 ; handler=loadGlobal };
                        { opCode=StoreGlobal ; opPopCount=1 ; opParamCount=1 ; handler=dupFunction } ]

let emptyCPU = { instructionPointer=0 ; cpuStack=List.empty ; lastOp=None ; globalVariables = Map.empty ; faultyState=false ; errorMessage = "" }

let instructionsMappedToOperations =
    List.fold (fun map x -> Map.add x.opCode x map) Map.empty allInstructions

let getInstructionInfoForOpCode opCode =
    let operation = Map.tryFind opCode instructionsMappedToOperations
    match operation with
    | Some operation -> Some operation
    | None           -> None

let incrementInstructionPointer cpu =
    { cpu with instructionPointer = cpu.instructionPointer + 1 }

let runOpCodeHandlerFunction instruction (instructionParams: int list) cpu =
    let cpu = cpu |> incrementInstructionPointer
    let (popped, newCpu) = popFromStack instruction.opPopCount cpu
    let hasCorrectAmountOfParams = instructionParams.Length = instruction.opParamCount
    match (hasCorrectAmountOfParams, popped) with
    | (false, _)            -> (cpu, sprintf "(%i) Invalid amount of params given for instruction %A" cpu.instructionPointer instruction.opCode) ||> setCPUToErrorState
    | (_, None)             -> newCpu // Too few in stack, error state
    | (_, Some (x::y::_))   -> instruction.handler (x, y) instructionParams newCpu
    | (_, Some (x::_))      -> instruction.handler (x, 0) instructionParams newCpu
    | (_, Some [])          -> instruction.handler (0, 0) instructionParams newCpu

let runSimpleInstruction operationAndParams cpu =
    let instructionInfo = getInstructionInfoForOpCode operationAndParams.operation
    match instructionInfo with
    | None -> cpu // error message, instruction not found
    | Some instructionInfo ->
        (instructionInfo, operationAndParams.opCodeParams, cpu) |||> runOpCodeHandlerFunction

let runInstruction instruction cpu =
    runSimpleInstruction instruction cpu

let instructionsToCPUCode instructionsToRun usedInstructions =
    match usedInstructions with
    | None -> { remainingOperations = instructionsToRun ; doneOperations = List.empty ; cache = Map.empty }
    | Some usedInstructions -> { remainingOperations = instructionsToRun ; doneOperations = usedInstructions ; cache = Map.empty }


let getNewInstruction (code:Code) cpu =
    let nextInstruction = cpu.instructionPointer
    let remainingLen = code.doneOperations.Length
    let cachedInstructions = Map.tryFind nextInstruction code.cache

    match (nextInstruction, code, cachedInstructions) with
    | (_, _, Some (remainingCode, usedCode) )                                ->
        { code with remainingOperations = remainingCode ; doneOperations = usedCode }
    | (nextInstruction, code, None) when nextInstruction > remainingLen ->
        let (cut, newInstructions) = code.remainingOperations |> List.splitAt (nextInstruction-remainingLen)
        let doneOperations = (cut@code.doneOperations)
        { code with remainingOperations = newInstructions ; doneOperations = doneOperations ; cache = Map.add nextInstruction (newInstructions, doneOperations) code.cache }
    | (nextInstruction, code, None) when nextInstruction < remainingLen ->
        let (newInstructions, cut) = code.doneOperations |> List.splitAt (remainingLen-nextInstruction)
        let remainingOperations = newInstructions@code.remainingOperations |> List.rev
        { code with remainingOperations = remainingOperations ; doneOperations = cut ; cache = Map.add nextInstruction (remainingOperations, cut) code.cache }
    | _                                                           ->
        code

let rec runCPU code cpu = 
    let code = getNewInstruction code cpu
    match (cpu.faultyState, code.remainingOperations) with
    | (true, _) ->
        printf "%s" cpu.errorMessage // Stop execution because CPU is in faulty state
        cpu
    | (_, [])   -> cpu
    | (_, currentInstruction::remainingInstructions) -> 
        runInstruction currentInstruction cpu |> runCPU { code with remainingOperations = remainingInstructions ; doneOperations = (currentInstruction::code.doneOperations) }

let getInstructionForLine (line: string) =
    let components = line.Split ' ' |> Seq.toList
    let instruction = components |> List.head
    let op = getInstructionFromString instruction
    match op with
    | Some op -> { operation = op ; opCodeParams = components |> List.tail |> List.map (fun x -> int x) }
    | None -> failwithf "Instruction %s not found" instruction
        

let convertCodeLinesToInstructions lines =
    try
        let operations = List.map (fun x -> getInstructionForLine x) lines
        instructionsToCPUCode operations None |> Some
    with
        | ex -> do eprintfn "%s" ex.Message
                None

let readCodeFromFile fileName  = 
    try 
        System.IO.File.ReadLines(fileName) |> seq<string> |> Some
    with 
        | ex -> do eprintfn "%s" ex.Message 
                None