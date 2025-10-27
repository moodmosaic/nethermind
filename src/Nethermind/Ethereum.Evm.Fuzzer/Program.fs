module Program

open SharpFuzz
open Ethereum.Evm.Logic

[<EntryPoint>]
let main _ =
    // SharpFuzz expects a real Main entrypoint here.
    Fuzzer.LibFuzzer.Run(fun data -> Logic.structure_harness data)
    0
