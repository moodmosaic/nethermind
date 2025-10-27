module Program

open System
open SharpFuzz
open Nethermind.Core
open Nethermind.Int256

let harness (data: ReadOnlySpan<byte>) : unit =
    try
        let tx = Transaction()
        tx.Data <- data.ToArray()
        tx.Value <- UInt256.Zero
        tx.GasLimit <- 1_000_000L
        tx.GasPrice <- UInt256.One
        tx.To <- null
        tx.Nonce <- 0UL

        let bytecode = tx.Data.Span
        if bytecode.Length > 0 then
            let mutable i = 0
            while i < bytecode.Length do
                let opcode = bytecode[i]
                if opcode >= 0x60uy && opcode <= 0x7Fuy then
                    let pushSize = int (opcode - 0x60uy + 1uy)
                    if i + pushSize >= bytecode.Length then
                        // Skip raising here — just break or ignore.
                        i <- bytecode.Length
                    else
                        i <- i + pushSize
                i <- i + 1

        // 🍌 Hidden branch.
        if data.Length >= 6 then
            if data[0] = byte 'b' &&
               data[1] = byte 'a' &&
               data[2] = byte 'n' &&
               data[3] = byte 'a' &&
               data[4] = byte 'n' &&
               data[5] = byte 'a' then
                raise (InvalidOperationException "Found banana!")
    with
    | :? InvalidOperationException -> ()


[<EntryPoint>]
let main _ =
    // SharpFuzz expects a real Main entrypoint here.
    Fuzzer.LibFuzzer.Run(fun data -> harness data)
    0
