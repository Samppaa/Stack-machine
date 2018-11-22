module Stack_machine
open System
open CPU

[<EntryPoint>]
let main argv =
    let codeAsText = readCodeFromFile "test.txt"
    match codeAsText with
    | Some codeAsText -> 
        let code = codeAsText |> Seq.toList |> convertCodeLinesToInstructions
        match code with
        | Some code ->
            try
                runCPU code emptyCPU |> printf "%A"
            with
                | ex -> eprintfn "%s" ex.Message
        | None -> ()
    | None -> ()
    0
